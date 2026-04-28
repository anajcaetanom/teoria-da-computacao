{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import GHC.Generics (Generic)
import Data.Yaml (FromJSON, parseJSON)
import Data.Aeson (Value(..), withObject, (.:), withArray)
import Data.Aeson.Types (Parser)
import qualified Data.Scientific as Sci
import qualified Data.Vector as V
import qualified Data.Text as T

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