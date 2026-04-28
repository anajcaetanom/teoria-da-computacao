{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Conversions.NfaToDfa where

import qualified Data.Text as T
import qualified Data.Set as S

import Conversions.NfaeToNfa
import Types

makeStateName :: S.Set T.Text -> T.Text
makeStateName stateSet 
    | S.null stateSet = "phi"
    | otherwise = "{" <> T.intercalate "," (S.toList stateSet) <> "}"

exploreDFA :: DeltaIndex -> [T.Text] -> S.Set (S.Set T.Text) -> [S.Set T.Text] -> [Transition] -> ([S.Set T.Text], [Transition])
exploreDFA _ _ visited [] accTrans = (S.toList visited, accTrans)
exploreDFA deltaIndex symbols visited (qSet:restQueue) accTrans
    | qSet `S.member` visited = exploreDFA deltaIndex symbols visited restQueue accTrans
    | otherwise =
        let newVisited = S.insert qSet visited
            
            moves = [ (sym, moveWithSymbol deltaIndex qSet sym) | sym <- symbols ]
            
            newTransitions = 
                [ Transition {
                    from = makeStateName qSet,
                    symbol = sym,
                    to = [makeStateName destSet]
                  } 
                | (sym, destSet) <- moves 
                ]
            
            newTargets = map snd moves
            unvisitedTargets = filter (`S.notMember` newVisited) newTargets
            newQueue = restQueue ++ unvisitedTargets
            
        in exploreDFA deltaIndex symbols newVisited newQueue (accTrans ++ newTransitions)

nfaToDFA :: Automata -> Automata
nfaToDFA nfa =
    let deltaIndex = buildDeltaIndex (transitions nfa)
        nfaSymbols = alphabet nfa
        
        initialDfaState = S.singleton (initialState nfa)
        
        (dfaStateSets, dfaTransitions) = exploreDFA deltaIndex nfaSymbols S.empty [initialDfaState] []
        
        dfaStateNames = map makeStateName dfaStateSets
        dfaInitialState = makeStateName initialDfaState
        
        oldFinals = S.fromList (finalStates nfa)
        isFinal set = not (S.null (S.intersection set oldFinals))
        dfaFinalStates = map makeStateName (filter isFinal dfaStateSets)
        
    in Automata {
        automataType = "dfa",
        alphabet = nfaSymbols,
        states = dfaStateNames,
        initialState = dfaInitialState,
        finalStates = dfaFinalStates,
        transitions = dfaTransitions
    }