{-# LANGUAGE OverloadedStrings #-}

module Regex.RegexToNfae where

import qualified Data.Text as T
import qualified Data.Set as S

import Types
import Regex.Types

data Fragment = Fragment { 
    startState :: T.Text,
    finalState :: T.Text,
    trans      :: [Transition],
    nextId     :: Int
} deriving (Show)

newState :: Int -> T.Text
newState n =
    "q" <> T.pack (show n)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates xs =
    S.toList (S.fromList xs)

regexAlphabet :: Regex -> [T.Text]
regexAlphabet Epsilon = []

regexAlphabet (Lit symbolText) = [symbolText]
regexAlphabet (Concat left right) = regexAlphabet left ++ regexAlphabet right
regexAlphabet (Union left right) = regexAlphabet left ++ regexAlphabet right
regexAlphabet (Star regex) = regexAlphabet regex
regexAlphabet (Plus regex) = regexAlphabet regex
regexAlphabet (Optional regex) = regexAlphabet regex

build :: Regex -> Int -> Fragment
build Epsilon n =
    let state = newState n
    in Fragment { 
        startState = state,
        finalState = state,
        trans = [],
        nextId = n + 1
    }

build (Lit symbolText) n =
    let start = newState n
        final = newState (n + 1)
    in Fragment { 
        startState = start,
        finalState = final,
        trans = [Transition start symbolText [final]],
        nextId = n + 2
    }

build (Concat left right) n =
    let leftFrag = build left n
        rightFrag = build right (nextId leftFrag)

        epsilonTrans =
            Transition
                (finalState leftFrag)
                "epsilon"
                [startState rightFrag]

    in Fragment { 
        startState = startState leftFrag,
        finalState = finalState rightFrag,
        trans = trans leftFrag ++ [epsilonTrans] ++ trans rightFrag,
        nextId = nextId rightFrag
    }

build (Union left right) n =
    let start = newState n
        leftFrag = build left (n + 1)
        rightFrag = build right (nextId leftFrag)
        final = newState (nextId rightFrag)

        startTrans =
            Transition
                start
                "epsilon"
                [startState leftFrag, startState rightFrag]

        leftFinalTrans =
            Transition
                (finalState leftFrag)
                "epsilon"
                [final]

        rightFinalTrans =
            Transition
                (finalState rightFrag)
                "epsilon"
                [final]

    in Fragment { 
        startState = start,
        finalState = final,
        trans =
            [startTrans]
            ++ trans leftFrag
            ++ trans rightFrag
            ++ [leftFinalTrans, rightFinalTrans],
        nextId = nextId rightFrag + 1
    }

build (Star regex) n =
    let start = newState n
        regexFrag = build regex (n + 1)
        final = newState (nextId regexFrag)

        startTrans =
            Transition
                start
                "epsilon"
                [startState regexFrag, final]

        loopTrans =
            Transition
                (finalState regexFrag)
                "epsilon"
                [startState regexFrag, final]

    in Fragment {
        startState = start,
        finalState = final,
        trans =
            [startTrans]
            ++ trans regexFrag
            ++ [loopTrans],
        nextId = nextId regexFrag + 1
    }

build (Plus regex) n =
    build (Concat regex (Star regex)) n

build (Optional regex) n =
    build (Union regex Epsilon) n

regexToNFAe :: Regex -> Automata
regexToNFAe regex =
    let finalFrag = build regex 0

        generatedStates =
            map newState [0 .. nextId finalFrag - 1]

        generatedAlphabet =
            removeDuplicates (regexAlphabet regex)

    in Automata { 
        automataType = "nfae",
        alphabet = generatedAlphabet,
        states = generatedStates,
        initialState = startState finalFrag,
        finalStates = [finalState finalFrag],
        transitions = trans finalFrag
    }