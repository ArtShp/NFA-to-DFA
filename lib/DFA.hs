{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module DFA where

import AutomataBase

type DFATransition q a = q -> a -> Maybe q

data (Show q, Show a) => DFA q a = DFA
    { states       :: States q
    , alphabet     :: Alphabet a
    , transition   :: DFATransition q a
    , initialState :: q
    , finalStates  :: States q
    }

data (Show q, Show a) => ValidDFA q a = ValidDFA
    { states       :: States q
    , alphabet     :: Alphabet a
    , transition   :: DFATransition q a
    , initialState :: q
    , finalStates  :: States q
    }

validateDFA :: (Eq q, Show q, Show a) => DFA q a -> Maybe (ValidDFA q a)
validateDFA dfa@(DFA states alphabet transition initialState finalStates) =
    if isValidDFA dfa
    then Just (ValidDFA states alphabet transition initialState finalStates)
    else Nothing

isValidDFA :: (Eq q, Show q, Show a) => DFA q a -> Bool
isValidDFA (DFA states alphabet transition initialState finalStates) =
    not (null states) &&
    not (null alphabet) &&
    initialState `elem` states &&
    all (`elem` states) finalStates &&
    all (\q -> all (\a -> case transition q a of
                            Just nextState -> nextState `elem` states
                            Nothing -> False) alphabet) states
