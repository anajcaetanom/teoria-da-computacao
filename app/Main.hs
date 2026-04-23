{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


import GHC.Generics (Generic)
import Data.Yaml (FromJSON, parseJSON, decodeFileEither, ParseException)
import Data.Aeson (Value(..), withObject, (.:), withArray)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Scientific as Sci

data Transition = Transition {
    from    :: T.Text,
    symbol  :: T.Text,
    to      :: [T.Text]
} deriving (Show, Generic)

data Automata = Automata {
    automataType   :: T.Text,
    alphabet       :: [T.Text],
    states         :: [T.Text],
    initialState   :: T.Text,
    finalStates    :: [T.Text],
    transitions    :: [Transition]
} deriving (Show, Generic)

scientificToText :: Sci.Scientific -> T.Text
scientificToText number =
    case Sci.floatingOrInteger number :: Either Double Integer of
        Right int -> T.pack (show int)
        Left    _ -> T.pack (Sci.formatScientific Sci.Fixed Nothing number)

parseTextOrNumber :: Value -> Parser T.Text
parseTextOrNumber (String s) = pure s
parseTextOrNumber (Number n) = pure (scientificToText n)
parseTextOrNumber _ = fail "esperado string ou number"

parseAlphabet :: Value -> Parser [T.Text]
parseAlphabet = withArray "alphabet" $ \arr ->
    mapM parseTextOrNumber (V.toList arr)
instance FromJSON Automata where
    parseJSON = withObject "Automata" $ \campo ->
        Automata
            <$>  campo .: "type"
            <*> (campo .: "alphabet" >>= parseAlphabet)
            <*>  campo .: "states"
            <*>  campo .: "initial_state"
            <*>  campo .: "final_states"
            <*>  campo .: "transitions"

instance FromJSON Transition where
    parseJSON = withObject "Transition" $ \campo ->
        Transition
            <$> campo .: "from"
            <*> (campo .: "symbol" >>= parseTextOrNumber)
            <*> campo .: "to"


---------------------------------------------------------------

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
---------------------------------------------------------------
commaList :: [T.Text] -> T.Text
commaList = T.intercalate ", "

renderOutputYAML :: Automata -> T.Text
renderOutputYAML automata =
    T.unlines [
        "type: " <> automataType automata,
        "alphabet: [" <> commaList (alphabet automata) <> "]",
        "states: [" <> commaList (states automata) <> "]",
        "initial_state: " <> initialState automata,
        "final_states: [" <> commaList (finalStates automata) <> "]",
        "transitions:"
    ]
    <> T.concat (map renderTransition (transitions automata))

renderTransition :: Transition -> T.Text
renderTransition transition =
    T.unlines [
        "  - from: " <> from transition,
        "    symbol: " <> symbol transition,
        "    to: [" <> commaList (to transition) <> "]"
    ]

main :: IO ()
main = do
    readResults <- decodeFileEither "entrada.yaml" :: IO (Either ParseException Automata)

    case readResults of 
        Left err ->
            putStrLn $ "Erro na leitura do arquivo de entrada: " ++ show err
        
        Right nfae -> do        
            let nfa = nfaeToNFA nfae
            let sorted = renderOutputYAML nfa
            TIO.writeFile "output.yaml" sorted

            
            
            