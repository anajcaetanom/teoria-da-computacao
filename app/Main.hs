{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


import GHC.Generics (Generic)
import Data.Yaml (FromJSON, ToJSON, parseJSON, decodeFileEither, encodeFile, ParseException)
import Data.Aeson (Value(..), withObject, (.:))
import qualified Data.Vector as V
import qualified Data.Text as T

data Transition = Transition {
    from    :: T.Text,
    symbol  :: T.Text,
    to      :: [T.Text]
} deriving (Show, Generic)

data Automata = Automata {
    automataType   :: T.Text,
    alphabet        :: [T.Text],
    states          :: [T.Text],
    initialState   :: T.Text,
    finalStates    :: [T.Text],
    transitions     :: [Transition]
} deriving (Show, Generic)



instance FromJSON Automata where
    parseJSON = withObject "Automata" $ \campo ->
        Automata
            <$> campo .: "type"
            <*> campo .: "alphabet"
            <*> campo .: "states"
            <*> campo .: "initial_state"
            <*> campo .: "final_states"
            <*> campo .: "transitions"

instance FromJSON Transition where
    parseJSON = withObject "Transition" $ \campo ->
        Transition
            <$> campo .: "from"
            <*> campo .: "symbol" 
            <*> campo .: "to"

-- funções auxiliares


-- TODO: código de conversão NFAɛ → NFA

main :: IO ()
main = do
    readResults <- decodeFileEither "entrada.yaml" :: IO (Either ParseException Automata)

    case readResults of 
        Left err ->
            putStrLn $ "Erro na leitura do arquivo de entrada: " ++ show err
        
        Right fileData -> do
            -- chama a função de converter e guarda num let
            -- escrita no YAML de saída
            
            ------teste------
            print fileData