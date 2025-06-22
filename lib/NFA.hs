module NFA 
    ( NFA, states, alphabet, transition, initialState, finalStates
    , makeNFA
    ) where

import AutomataBase

-- We use `Nothing` here to represent an ε-move.
type NFATransition q a = q -> Maybe a -> States q

-- Assumed to be valid
data NFA q a = NFA
    { states       :: States q
    , alphabet     :: Alphabet a
    , transition   :: NFATransition q a
    , initialState :: q
    , finalStates  :: States q
    }

isValidNFA :: Eq q => NFA q a -> Bool
isValidNFA (NFA qs as trans q0 fs) =
    not (null qs) &&
    not (null as) &&
    q0 `elem` qs &&
    all (`elem` qs) fs &&
    all (\q -> all (isValidTransition q) wrappedAlphabet) qs
    where
        isValidTransition q input = all (`elem` qs) (trans q input)
        wrappedAlphabet = Nothing : map Just as

makeNFA :: Eq q => States q -> Alphabet a -> NFATransition q a -> q -> States q -> Maybe (NFA q a)
makeNFA qs as trans q0 fs
    | isValidNFA nfa = Just nfa
    | otherwise      = Nothing
        where
            nfa = NFA qs as trans q0 fs
