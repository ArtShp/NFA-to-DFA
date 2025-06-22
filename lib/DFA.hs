module DFA 
    ( DFA, states, alphabet, transition, initialState, finalStates
    , makeDFA
    ) where

import AutomataBase

type DFATransition q a = q -> a -> Maybe q

-- Assumed to be valid
data DFA q a = DFA
    { states       :: States q
    , alphabet     :: Alphabet a
    , transition   :: DFATransition q a
    , initialState :: q
    , finalStates  :: States q
    }

isValidDFA :: Eq q => DFA q a -> Bool
isValidDFA (DFA qs as trans q0 fs) =
    not (null qs) &&
    not (null as) &&
    q0 `elem` qs &&
    all (`elem` qs) fs &&
    all (\q -> all (\a -> case trans q a of
                            Just nextState -> nextState `elem` qs
                            Nothing -> True) as) qs

makeDFA :: Eq q => States q -> Alphabet a -> DFATransition q a -> q -> States q -> Maybe (DFA q a)
makeDFA qs as trans q0 fs
    | isValidDFA dfa = Just dfa
    | otherwise      = Nothing
        where
            dfa = DFA qs as trans q0 fs
