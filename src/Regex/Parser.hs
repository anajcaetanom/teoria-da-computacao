{-# LANGUAGE OverloadedStrings #-}

module Regex.Parser where

import qualified Data.Text as T
import Data.List (isPrefixOf)

import Regex.Types

parseRegexText :: T.Text -> Either String Regex
parseRegexText input =
    case parseUnion (T.unpack input) of
        Right (regex, rest) ->
            if null rest
                then Right regex
                else Left ("Entrada não consumida: " ++ rest)

        Left err ->
            Left err

parseUnion :: String -> Either String (Regex, String)
parseUnion input = do
    (left, rest) <- parseConcat input

    case rest of
        ('|':afterPipe) -> do
            (right, rest2) <- parseUnion afterPipe
            Right (Union left right, rest2)

        _ ->
            Right (left, rest)

parseConcat :: String -> Either String (Regex, String)
parseConcat input = do
    (first, rest) <- parsePostfix input
    parseMoreConcat first rest

parseMoreConcat :: Regex -> String -> Either String (Regex, String)
parseMoreConcat acc rest =
    case rest of
        ""      -> Right (acc, "")
        (')':_) -> Right (acc, rest)
        ('|':_) -> Right (acc, rest)

        _ -> do
            (next, rest2) <- parsePostfix rest
            parseMoreConcat (Concat acc next) rest2

parsePostfix :: String -> Either String (Regex, String)
parsePostfix input = do
    (base, rest) <- parseAtom input
    parsePostfixOps base rest

parsePostfixOps :: Regex -> String -> Either String (Regex, String)
parsePostfixOps regex rest =
    case rest of
        ('*':afterStar) ->
            parsePostfixOps (Star regex) afterStar

        ('+':afterPlus) ->
            parsePostfixOps (Plus regex) afterPlus

        ('?':afterQuestion) ->
            parsePostfixOps (Optional regex) afterQuestion

        _ ->
            Right (regex, rest)

parseAtom :: String -> Either String (Regex, String)
parseAtom input =
    case input of
        "" ->
            Left "Expressão incompleta"

        ('(':afterOpen) -> do
            (inside, rest) <- parseUnion afterOpen

            case rest of
                (')':afterClose) ->
                    Right (inside, afterClose)

                _ ->
                    Left "Parêntese não fechado"

        _ | "epsilon" `isPrefixOf` input ->
            Right (Epsilon, drop 7 input)

        (currentChar:rest)
            | currentChar `elem` ['|', '*', '+', '?', ')'] ->
                Left ("Símbolo inesperado: " ++ [currentChar])

            | otherwise ->
                Right (Lit (T.singleton currentChar), rest)