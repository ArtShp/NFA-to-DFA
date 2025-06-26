module DFA 
    ( DFA(..)
    , makeDFA
    , DFATransition
    ) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import AutomataBase

type DFATransition q a = Map.Map (q, a) q -- = q -> a -> Maybe q

-- Assumed to be valid
data DFA q a = DFA
    { states       :: States q
    , alphabet     :: Alphabet a
    , transition   :: DFATransition q a
    , initialState :: q
    , finalStates  :: States q
    }

isValidDFA :: (Ord q, Ord a) => DFA q a -> Bool
isValidDFA (DFA qs as trans q0 fs) =
    not (Set.null qs) &&
    not (Set.null as) &&
    q0 `Set.member` qs &&
    fs `Set.isSubsetOf` qs &&
    all (\(q, a) -> case Map.lookup (q, a) trans of
            Just nextState -> nextState `Set.member` qs
            Nothing        -> True)
        [(q, a) | q <- Set.toList qs, a <- Set.toList as]

makeDFA :: (Ord q, Ord a) => States q -> Alphabet a -> DFATransition q a -> q -> States q -> Maybe (DFA q a)
makeDFA qs as trans q0 fs =
    let dfa = DFA qs as trans q0 fs
    in if isValidDFA dfa then Just dfa else Nothing
