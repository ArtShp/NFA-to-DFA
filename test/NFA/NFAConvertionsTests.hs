module NFA.NFAConvertionsTests (nfaConvertionsTests) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Test.HUnit

import AutomataBase
import NFA
import qualified DFA
import Convertions.NFAConvertions

nfaConvertionsTests :: [Test]
nfaConvertionsTests =
    [ TestLabel "epsilon-closure chain"  testEpsilonClosureChain
    , TestLabel "epsilon-closure no eps" testEpsilonClosureNoEps
    , TestLabel "move symbol"            testMoveSymbol
    , TestLabel "move closure"           testMoveClosure
    , TestLabel "toDFA simple"           testNfaToDfaSimple
    , TestLabel "toDFA with epsilon"     testNfaToDfaWithEps
    ]

-- A tiny NFA for testing epsilonClosure and move:
-- 0 -  ε  -> 1 - ε -> 2
-- 1 - 'a' -> 3
transEps :: NFATransition Int Char
transEps = Map.fromList
    [ ((0, Nothing), Set.singleton 1)
    , ((1, Nothing), Set.singleton 2)
    , ((1, Just 'a'), Set.singleton 3)
    ]

qs1, qs3 :: States Int
qs1 = Set.singleton 1
qs3 = Set.singleton 3

testEpsilonClosureChain :: Test
testEpsilonClosureChain = TestCase $
    let ec = epsilonClosure transEps (Set.singleton 0)
    in assertEqual "epsilon-closure of {0}" (Set.fromList [0, 1, 2]) ec

testEpsilonClosureNoEps :: Test
testEpsilonClosureNoEps = TestCase $
    let ec = epsilonClosure transEps qs3
    in assertEqual "epsilon-closure of {3} with no outgoing epsilon" qs3 ec

testMoveSymbol :: Test
testMoveSymbol = TestCase $
    let mv = move transEps qs1 'a'
    in assertEqual "move from {1} on 'a'" qs3 mv

testMoveClosure :: Test
testMoveClosure = TestCase $
    let mc1 = moveClosure transEps 1 (Just 'a')
        mc0 = moveClosure transEps 0 Nothing
    in do
        assertEqual "moveClosure 1 'a'" qs3 mc1
        assertEqual "moveClosure 0 epsilon" (Set.fromList [0, 1, 2]) mc0

-- A simple NFA without epsilon for testing nfaToDfa:
-- states {0,1}, alphabet {'a'}, 0 -'a'-> 1, initial=0, final={1}
transSimple :: NFATransition Int Char
transSimple = Map.singleton (0, Just 'a') (Set.singleton 1)

nfaSimple :: NFA Int Char
nfaSimple = NFA (Set.fromList [0, 1]) (Set.singleton 'a') transSimple 0 (Set.singleton 1)

expectedDfaSimple :: DFA.DFA (States Int) Char
expectedDfaSimple =
    let s0 = Set.singleton 0
        s1 = Set.singleton 1
        sigma = Set.singleton 'a'
        delta = Map.fromList [((s0, 'a'), s1)]
        finals = Set.singleton s1
    in DFA.DFA (Set.fromList [s0, s1]) sigma delta s0 finals

testNfaToDfaSimple :: Test
testNfaToDfaSimple = TestCase $
    assertEqual "convert simple NFA to DFA" expectedDfaSimple (nfaToDfa nfaSimple)

-- An NFA with epsilon and symbol transitions for full conversion test:
-- 0 -ε-> 1, 1 -'a'-> 2, initial=0, final={2}
transMix :: NFATransition Int Char
transMix = Map.fromList
    [ ((0, Nothing), Set.singleton 1)
    , ((1, Just 'a'), Set.singleton 2)
    ]

nfaMix :: NFA Int Char
nfaMix = NFA (Set.fromList [0, 1, 2]) (Set.singleton 'a') transMix 0 (Set.singleton 2)

testNfaToDfaWithEps :: Test
testNfaToDfaWithEps = TestCase $
    let dfa = nfaToDfa nfaMix
        s01 = Set.fromList [0, 1]
        s2  = Set.singleton 2
        expectedStates = Set.fromList [s01, s2]
        expectedSigma  = Set.singleton 'a'
        expectedDelta  = Map.fromList [((s01, 'a'), s2)]
        expectedFinals = Set.singleton s2
        expectedDfa = DFA.DFA expectedStates expectedSigma expectedDelta s01 expectedFinals
    in do
        assertEqual "convert mix NFA to DFA" expectedDfa dfa
