module Simulators.NFASimulators where

import qualified Data.Set as Set

import AutomataBase
import NFA
import Convertions.NFAConvertions as NFAConvertions

nfaSimulateOneStepOneState :: (Ord q, Ord a) => NFA q a -> q -> Maybe a -> States q
nfaSimulateOneStepOneState nfa q a = moveClosure (transition nfa) q a

nfaSimulateOneStep :: (Ord q, Ord a) => NFA q a -> States q -> Maybe a -> States q
nfaSimulateOneStep nfa qs a = Set.unions [nfaSimulateOneStepOneState nfa q a | q <- Set.toList qs]

nfaSimulateFromState :: (Ord q, Ord a) => NFA q a -> q -> [a] -> States q
nfaSimulateFromState nfa q input =
    let initStates = epsilonClosure (transition nfa) (Set.singleton q)
    in go initStates input
    where
        go current []     = current
        go current (a:as) =
            let next = nfaSimulateOneStep nfa current (Just a)
            in if Set.null next then Set.empty else go next as

nfaSimulateFromStateWithHistory :: (Ord q, Ord a) => NFA q a -> q -> [a] -> Set.Set ([(q, Maybe a)], States q)
nfaSimulateFromStateWithHistory nfa start input =
    let trans = transition nfa
        initBranches =
            ([], start)
            : [([(start, Nothing)], s)
                | s <- Set.toList (epsilonClosure trans (Set.singleton start))
                , s /= start
              ]

        step branches a =
            [ (hist ++ [(s, Just a)], s')
            | (hist, s) <- branches, s' <- Set.toList (moveClosure trans s (Just a))
            ]

        finalBranches = foldl step initBranches input

    in Set.fromList
        [ (hist, epsilonClosure trans (Set.singleton endState))
        | (hist, endState) <- finalBranches, not (null hist)
        ]

nfaSimulateFromStart :: (Ord q, Ord a) => NFA q a -> [a] -> States q
nfaSimulateFromStart nfa = nfaSimulateFromState nfa (initialState nfa)

nfaSimulateFromStartWithHistory :: (Ord q, Ord a) => NFA q a -> [a] -> Set.Set ([(q, Maybe a)], States q)
nfaSimulateFromStartWithHistory nfa = nfaSimulateFromStateWithHistory nfa (initialState nfa)

nfaIsAccepted :: (Ord q, Ord a) => NFA q a -> [a] -> Bool
nfaIsAccepted nfa input =
    let reached = nfaSimulateFromStart nfa input
    in not (Set.null (Set.intersection (finalStates nfa) reached))
