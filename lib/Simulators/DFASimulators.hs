module Simulators.DFASimulators where

import qualified Data.Set as Set
import qualified Data.Map as Map

import DFA

dfaSimulateOneStep :: (Ord q, Ord a) => DFA q a -> q -> a -> Maybe q
dfaSimulateOneStep dfa q a = Map.lookup (q, a) (dfaTransition dfa)

dfaSimulateFromState :: (Ord q, Ord a) => DFA q a -> q -> [a] -> Maybe q
dfaSimulateFromState dfa = go
    where
        go current [] = Just current
        go current (a:as) =
            case dfaSimulateOneStep dfa current a of
                Just nextState -> go nextState as
                Nothing        -> Nothing

dfaSimulateFromStateWithHistory :: (Ord q, Ord a) => DFA q a -> q -> [a] -> ([(q, a)], Maybe q)
dfaSimulateFromStateWithHistory dfa q input = go q input []
    where
        go current [] history = (reverse history, Just current)
        go current (a:as) history =
            case dfaSimulateOneStep dfa current a of
                Just nextState -> go nextState as ((current, a):history)
                Nothing        -> (reverse ((current, a):history), Nothing)

dfaSimulateFromStart :: (Ord q, Ord a) => DFA q a -> [a] -> Maybe q
dfaSimulateFromStart dfa = dfaSimulateFromState dfa (dfaInitialState dfa)

dfaSimulateFromStartWithHistory :: (Ord q, Ord a) => DFA q a -> [a] -> ([(q, a)], Maybe q)
dfaSimulateFromStartWithHistory dfa = dfaSimulateFromStateWithHistory dfa (dfaInitialState dfa)

dfaIsAccepted :: (Ord q, Ord a) => DFA q a -> [a] -> Bool
dfaIsAccepted dfa input =
    case dfaSimulateFromStart dfa input of
        Just q  -> q `Set.member` dfaFinalStates dfa
        Nothing -> False
