module Convertions.NFAConvertions (nfaToDfa) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AutomataBase
import qualified DFA
import NFA

epsilonClosure :: (Ord q, Ord a) => NFATransition q a -> States q -> States q
epsilonClosure trans qs = closure qs Set.empty
    where
        closure toVisit visited
            | Set.null toVisit = visited
            | otherwise =
                let s = Set.elemAt 0 toVisit
                    rest = Set.deleteAt 0 toVisit
                    visited' = Set.insert s visited
                    next = Map.findWithDefault Set.empty (s, Nothing) trans
                    newStates = Set.difference next visited'
                in closure (Set.union rest newStates) visited'

move :: (Ord q, Ord a) => NFATransition q a -> States q -> a -> States q
move trans qs symbol =
    Set.unions [Map.findWithDefault Set.empty (s, Just symbol) trans | s <- Set.toList qs]

nfaToDfa :: (Ord q, Ord a) => NFA q a -> DFA.DFA (States q) a
nfaToDfa nfa = go Set.empty [initState] Map.empty
    where
        trans = transition nfa
        as = alphabet nfa
        fs = finalStates nfa
        initState = epsilonClosure trans (Set.singleton $ initialState nfa)
        
        go visited [] delta = deleteEmptyState $ DFA.DFA visited as delta initState (Set.filter (not . Set.null . Set.intersection fs) visited)
        go visited (q:queue) delta
            | q `Set.member` visited = go visited queue delta
            | otherwise =
                let visited' = Set.insert q visited
                    (delta', newStates) = foldl
                        (\(d, ns) a ->
                            let target = epsilonClosure trans (move trans q a)
                            in (Map.insert (q, a) target d, if target `Set.member` visited' then ns else target:ns)
                        ) (delta, []) (Set.toList as)
                in go visited' (queue ++ newStates) delta'

deleteEmptyState :: (Ord q, Ord a) => DFA.DFA (States q) a -> DFA.DFA (States q) a
deleteEmptyState dfa@DFA.DFA{DFA.states = qs, DFA.alphabet = sigma, DFA.transition = trans, DFA.initialState = q0, DFA.finalStates = fs}
    | Set.empty `Set.notMember` qs = dfa
    | otherwise = DFA.DFA (Set.delete Set.empty qs) sigma 
                                         (Map.filterWithKey (\_ v -> not (Set.null v)) trans) q0 fs
