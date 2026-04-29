{-# LANGUAGE OverloadedStrings #-}

module Regex.Types where

import qualified Data.Text as T

data Regex
    = Lit T.Text
    | Epsilon
    | Concat Regex Regex
    | Union Regex Regex
    | Star Regex
    | Plus Regex
    | Optional Regex
    deriving (Show)