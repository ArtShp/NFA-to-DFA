module Convertions.DFAConvertions where

import qualified Data.Map as Map
import qualified Data.Set as Set

import DFA
import qualified NFA

dfaToNfa :: (Ord q, Ord a) => DFA q a -> NFA.NFA q a
dfaToNfa dfa = NFA.NFA
    { NFA.states       = states dfa
    , NFA.alphabet     = alphabet dfa
    , NFA.transition   = Map.fromAscList $ map (\((q1, a), q2) -> ((q1, Just a), Set.singleton q2)) (Map.toAscList (transition dfa))
    , NFA.initialState = initialState dfa
    , NFA.finalStates  = finalStates dfa
    }
