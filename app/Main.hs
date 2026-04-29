{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Yaml (decodeFileEither, ParseException)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Conversions.NfaeToNfa
import Conversions.NfaToDfa
import Types
import Render
import Regex.RegexToNfae
import Regex.Parser


main :: IO ()
main = do
    putStrLn "1 - Converter automato YAML"
    putStrLn "2 - Converter regex para NFAe"
    putStrLn "Escolha uma das opções: "
    op <- getLine

    case op of
        "1" -> convertYamlAutomata
        "2" -> convertRegexToNfae
        _   -> putStrLn "Opção inválida."

convertYamlAutomata :: IO()
convertYamlAutomata = do
    readResults <- decodeFileEither "entrada.yaml" :: IO (Either ParseException Automata)

    case readResults of 
        Left err -> 
            putStrLn $ "Erro na leitura do arquivo de entrada: " ++ show err

        Right automata -> do
            let result = 
                    case automataType automata of
                        "nfae" -> nfaeToNFA automata
                        "nfa"  -> nfaToDFA automata
                        "dfa"  -> automata
                        _      -> error "Tipo desconhecido."

            TIO.writeFile "output.yaml" (renderOutputYAML result)

convertRegexToNfae :: IO ()
convertRegexToNfae = do
    putStrLn "Digite a expressão regular: "
    regexText <- T.strip <$> TIO.getLine

    case parseRegexText regexText of
        Left err ->
            putStrLn $ "Erro ao ler regex: " ++ err

        Right regex -> do
            let nfae = regexToNFAe regex
            TIO.writeFile "output.yaml" (renderOutputYAML nfae)
            
            