{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where

import qualified Data.Text as T

import Types


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