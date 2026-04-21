{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


import GHC.Generics (Generic)
import Data.Yaml (FromJSON, ToJSON, parseJSON, decodeFileEither, encodeFile, ParseException)
import Data.Aeson (Value(..), withObject, (.:), withArray)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.Vector as V

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


parseAlphabet :: Value -> Parser [T.Text]
parseAlphabet = withArray "alphabet" $ \arr ->
    mapM parseTextOrNumber (V.toList arr)
instance FromJSON Automata where
    parseJSON = withObject "Automata" $ \campo ->
        Automata
            <$> campo .: "type"
            <*> (campo .: "alphabet" >>= parseAlphabet)
            <*> campo .: "states"
            <*> campo .: "initial_state"
            <*> campo .: "final_states"
            <*> campo .: "transitions"

instance FromJSON Transition where
    parseJSON = withObject "Transition" $ \campo ->
        Transition
            <$> campo .: "from"
            <*> (campo .: "symbol" >>= parseTextOrNumber)
            <*> campo .: "to"

-- funções auxiliares

parseTextOrNumber :: Value -> Parser T.Text
parseTextOrNumber (String s) = pure s
parseTextOrNumber (Number n) = pure (T.pack (show n))
parseTextOrNumber _ = fail "esperado string ou number"
        

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