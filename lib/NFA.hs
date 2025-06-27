module NFA 
    ( NFA(..)
    , makeNFA
    , NFATransition
    ) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import AutomataBase

-- We use `Nothing` here to represent an ε-move.
type NFATransition q a = Map.Map (q, Maybe a) (States q) -- q -> Maybe a -> States q

-- Assumed to be valid
data NFA q a = NFA
    { states       :: States q
    , alphabet     :: Alphabet a
    , transition   :: NFATransition q a
    , initialState :: q
    , finalStates  :: States q
    } deriving (Show, Eq)

isValidNFA :: (Ord q, Ord a) => NFA q a -> Bool
isValidNFA (NFA qs as trans q0 fs) =
    not (Set.null qs) &&
    not (Set.null as) &&
    q0 `Set.member` qs &&
    fs `Set.isSubsetOf` qs &&
    all (\q -> all (isValidTransition q) wrappedAlphabet) (Set.toList qs)
    where
        isValidTransition q input =
            Set.isSubsetOf (Map.findWithDefault Set.empty (q, input) trans) qs
        wrappedAlphabet = Nothing : map Just (Set.toList as)

makeNFA :: (Ord q, Ord a) => States q -> Alphabet a -> NFATransition q a -> q -> States q -> Maybe (NFA q a)
makeNFA qs as trans q0 fs =
    let nfa = NFA qs as trans q0 fs
    in if isValidNFA nfa then Just nfa else Nothing
