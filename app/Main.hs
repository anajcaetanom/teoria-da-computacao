{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Yaml (decodeFileEither, ParseException)
import qualified Data.Text.IO as TIO

import Conversions.NfaeToNfa
import Conversions.NfaToDfa
import Types
import Render


main :: IO ()
main = do
    readResults <- decodeFileEither "entrada.yaml" :: IO (Either ParseException Automata)

    case readResults of 
        Left err ->
            putStrLn $ "Erro na leitura do arquivo de entrada: " ++ show err
        
        Right automata -> do    
            let result = 
                    case automataType automata of
                        "nfae" -> nfaeToNFA automata
                        "nfa"  -> nfaToDFA automata
                        _      -> error "Tipo desconhecido." 

            TIO.writeFile "output.yaml" (renderOutputYAML result)

            
            
            