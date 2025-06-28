module Convertions.DFAConvertions where

import qualified Data.Map as Map
import qualified Data.Set as Set

import DFA
import NFA

dfaToNfa :: (Ord q, Ord a) => DFA q a -> NFA q a
dfaToNfa dfa = NFA
    { nfaStates       = dfaStates dfa
    , nfaAlphabet     = dfaAlphabet dfa
    , nfaTransition   = Map.fromAscList $ map (\((q1, a), q2) -> ((q1, Just a), Set.singleton q2)) (Map.toAscList (dfaTransition dfa))
    , nfaInitialState = dfaInitialState dfa
    , nfaFinalStates  = dfaFinalStates dfa
    }
