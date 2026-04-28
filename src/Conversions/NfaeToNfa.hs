{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Conversions.NfaeToNfa where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Types


isEpsilon :: T.Text -> Bool
isEpsilon symbolText = symbolText == "epsilon"

type DeltaIndex = M.Map (T.Text, T.Text) [T.Text]
buildDeltaIndex :: [Transition] -> DeltaIndex
buildDeltaIndex transitionsList =
    M.fromListWith (++) 
        [ ((from transitionItem, symbol transitionItem), to transitionItem)
        | transitionItem <- transitionsList ]

getTargets :: DeltaIndex -> T.Text -> T.Text -> [T.Text]
getTargets deltaIndex originState inputSymbol =
    M.findWithDefault [] (originState, inputSymbol) deltaIndex

getEpsilonTargets :: DeltaIndex -> T.Text -> [T.Text]
getEpsilonTargets deltaIndex originState = 
    getTargets deltaIndex originState "epsilon"
        
eClosureFromState ::DeltaIndex -> T.Text -> S.Set T.Text
eClosureFromState deltaIndex startState =
    visitStates S.empty [startState]
    where 
        visitStates visited queue
            | null queue = visited
            | otherwise =
                let currentState = head queue
                    restQueue = tail queue
                in if currentState `S.member` visited
                    then 
                        visitStates visited restQueue
                    else
                        let epsilonNeighbors = getEpsilonTargets deltaIndex currentState
                            newVisited = S.insert currentState visited
                            newQueue = restQueue ++ epsilonNeighbors
                        in 
                            visitStates newVisited newQueue

moveWithSymbol :: DeltaIndex -> S.Set T.Text -> T.Text -> S.Set T.Text
moveWithSymbol deltaIndex originStates inputSymbol = 
    S.fromList
        (concatMap
            (\originState -> getTargets deltaIndex originState inputSymbol)
            (S.toList originStates)
        )

eClosureFromSet :: DeltaIndex -> S.Set T.Text -> S.Set T.Text
eClosureFromSet deltaIndex originStates =
    S.unions
        (map
            (\originState -> eClosureFromState deltaIndex originState)
            (S.toList originStates)
        )

deltaPrime :: DeltaIndex -> T.Text -> T.Text -> S.Set T.Text
deltaPrime deltaIndex originState inputSymbol =
    let closureOfOriginState = eClosureFromState deltaIndex originState
        movedStates = moveWithSymbol deltaIndex closureOfOriginState inputSymbol
        closureAfterMove = eClosureFromSet deltaIndex movedStates
    in closureAfterMove

buildNewTransitions :: Automata -> DeltaIndex -> [Transition]
buildNewTransitions nfae deltaIndex = 
    concatMap (filter (not . null . to) . fromState) (states nfae)
    where
        withoutEpsilon =
            filter (not . isEpsilon) (alphabet nfae)
        
        fromState :: T.Text -> [Transition]
        fromState originState =
            map (fromStateAndSymbol originState) withoutEpsilon

        fromStateAndSymbol :: T.Text -> T.Text -> Transition
        fromStateAndSymbol originState inputSymbol =
            let targetStatesSet = 
                    deltaPrime deltaIndex originState inputSymbol
                targetStatesList = S.toList targetStatesSet
            in 
                Transition {
                    from = originState,
                    symbol = inputSymbol,
                    to = targetStatesList
                }

buildNewFinalStates :: Automata -> DeltaIndex -> [T.Text]
buildNewFinalStates nfae deltaIndex =
    filter becomesFinal (states nfae)
    where
        oldFinals = S.fromList (finalStates nfae)

        becomesFinal :: T.Text -> Bool
        becomesFinal stateName = 
            let closureSet = eClosureFromState deltaIndex stateName
            in not (S.null (S.intersection closureSet oldFinals))

nfaeToNFA :: Automata -> Automata
nfaeToNFA nfae =
    let deltaIndex = buildDeltaIndex (transitions nfae)
        newTransitions = buildNewTransitions nfae deltaIndex
        newFinalStates = buildNewFinalStates nfae deltaIndex
        newAlphabet = filter (not . isEpsilon) (alphabet nfae)
    in 
        Automata {
            automataType = "nfa",
            alphabet = newAlphabet,
            states = states nfae,
            initialState = initialState nfae,
            finalStates = newFinalStates,
            transitions = newTransitions
        }