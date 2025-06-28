module NFA.NFASimulatorsTests (nfaSimulatorsTests) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Test.HUnit

import Automata

nfaSimulatorsTests :: [Test]
nfaSimulatorsTests =
    [ TestLabel "epsilon-step from 0"  testEpsilonStep
    , TestLabel "one step on 'a'"      testOneStepA
    , TestLabel "simulate \"ab\""      testFromStateAB
    , TestLabel "history for \"ab\""   testHistoryAB
    , TestLabel "fromStart \"ab\""     testFromStart
    , TestLabel "start history \"ab\"" testFromStartHist
    , TestLabel "accept \"ab\""        testAccept
    , TestLabel "reject \"aa\""        testReject
    ]

-- A complex NFA:
--   0 -  ε  -> {1,2}
--   1 - 'a' -> {1}
--   1 - 'b' -> {3}
--   2 - 'a' -> {2}
--   2 - 'b' -> {2}
--   3 - 'b' -> {1}

statesC :: States Int
statesC  = Set.fromList [0, 1, 2, 3]

alphabetC :: Alphabet Char
alphabetC = Set.fromList "ab"

transC :: NFATransition Int Char
transC = Map.fromList
    [ ((0, Nothing),   Set.fromList [1, 2])
    , ((1, Just 'a'),  Set.singleton 1)
    , ((1, Just 'b'),  Set.singleton 3)
    , ((2, Just 'a'),  Set.singleton 2)
    , ((2, Just 'b'),  Set.singleton 2)
    , ((3, Just 'b'),  Set.singleton 1)
    ]

initialC :: Int
initialC = 0

finalsC :: States Int
finalsC = Set.singleton 3

-- Construct the NFA (it must be valid)
nfaC :: NFA Int Char
nfaC = NFA statesC alphabetC transC initialC finalsC

-- Test epsilon‐step from state 0
testEpsilonStep :: Test
testEpsilonStep = TestCase $
    let eps0 = nfaSimulateOneStepOneState nfaC 0 Nothing
    in assertEqual "epsilon-closure of 0" 
        (Set.fromList [0, 1, 2])
        eps0

-- Test one symbol step from {0,1,2} on 'a'
testOneStepA :: Test
testOneStepA = TestCase $
    let initSet = Set.fromList [0,1,2]
        stepA   = nfaSimulateOneStep nfaC initSet (Just 'a')
    in assertEqual "one step on 'a'" 
        (Set.fromList [1, 2])
        stepA

-- Test full simulate from 0 with "ab"
testFromStateAB :: Test
testFromStateAB = TestCase $
    assertEqual "simulate \"ab\" from 0"
        (Set.fromList [2, 3])
        (nfaSimulateFromState nfaC 0 "ab")

-- Test BFS‐history for "ab"
testHistoryAB :: Test
testHistoryAB = TestCase $
    let result   = nfaSimulateFromStateWithHistory nfaC 0 "ab"
        expected = Set.fromList
            [ ( [(0, Nothing), (1, Just 'a'), (1, Just 'b')]
              , Set.singleton 3
              )
            , ( [(0, Nothing), (2, Just 'a'), (2, Just 'b')]
              , Set.singleton 2
              )
            ]
    in assertEqual "history BFS for \"ab\"" expected result

-- Test simulate from start
testFromStart :: Test
testFromStart = TestCase $
    assertEqual "fromStart \"ab\""
        (Set.fromList [2, 3])
        (nfaSimulateFromStart nfaC "ab")

-- Test start with history
testFromStartHist :: Test
testFromStartHist = TestCase $
    let result   = nfaSimulateFromStartWithHistory nfaC "ab"
        expected = Set.fromList
            [ ( [(0, Nothing), (1, Just 'a'), (1, Just 'b')]
              , Set.singleton 3
              )
            , ( [(0, Nothing), (2, Just 'a'), (2, Just 'b')]
              , Set.singleton 2
              )
            ]
    in assertEqual "start history BFS for \"ab\"" expected result

-- Acceptance tests
testAccept :: Test
testAccept = TestCase $
    assertBool "NFA accepts \"ab\"" 
        (nfaIsAccepted nfaC "ab")

testReject :: Test
testReject = TestCase $
    assertBool "NFA rejects \"aa\"" 
        (not (nfaIsAccepted nfaC "aa"))
