module NFA.NFAValidationTests (nfaValidationTests) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Test.HUnit
import Data.Maybe (isJust, isNothing)

import Automata

nfaValidationTests :: [Test]
nfaValidationTests =
    [ TestLabel "valid simple NFA"            testValidNFA
    , TestLabel "empty states"                testEmptyStates
    , TestLabel "empty alphabet"              testEmptyAlphabet
    , TestLabel "initial not in states"       testInitialNotInStates
    , TestLabel "finals not subset of states" testFinalsNotSubset
    , TestLabel "transition outside states"   testInvalidTransition
    , TestLabel "complex valid NFA"           testComplexValid
    , TestLabel "complex invalid transition"  testComplexInvalidTrans
    ]

-- Common pieces for tests:
states_ :: States Int
states_ = Set.fromList [0, 1]

alphabet_ :: Alphabet Char
alphabet_ = Set.fromList "ab"

initial :: Int
initial = 0

finals :: States Int
finals  = Set.fromList [1]

-- valid transition: for every (q, ε or symbol) map back to q
validTrans :: NFATransition Int Char
validTrans = Map.fromList
    [ ((q, a), Set.singleton q)
    | q <- Set.toList states_, a <- Nothing : map Just (Set.toList alphabet_)
    ]

-- invalid transition: one move goes outside of states
badTrans :: NFATransition Int Char
badTrans = Map.insert (0, Just 'a') (Set.fromList [2]) validTrans

-- Tests:

testValidNFA :: Test
testValidNFA = TestCase $
    assertBool "makeNFA should succeed for a simple valid NFA"
        (isJust (makeNFA states_ alphabet_ validTrans initial finals))

testEmptyStates :: Test
testEmptyStates = TestCase $
    assertBool "makeNFA should fail on empty states"
        (isNothing (makeNFA Set.empty alphabet_ validTrans initial finals))

testEmptyAlphabet :: Test
testEmptyAlphabet = TestCase $
    assertBool "makeNFA should fail on empty alphabet"
        (isNothing (makeNFA states_ Set.empty validTrans initial finals))

testInitialNotInStates :: Test
testInitialNotInStates = TestCase $
    assertBool "makeNFA should fail if initialState is not in states"
        (isNothing (makeNFA states_ alphabet_ validTrans 42 finals))

testFinalsNotSubset :: Test
testFinalsNotSubset = TestCase $
    assertBool "makeNFA should fail if finalStates is not a subset of states"
        (isNothing (makeNFA states_ alphabet_ validTrans initial (Set.fromList [2])))

testInvalidTransition :: Test
testInvalidTransition = TestCase $
    assertBool "makeNFA should fail if some transition maps outside of states"
        (isNothing (makeNFA states_ alphabet_ badTrans initial finals))

-- A more complex NFA:
--   0 -  ε  -> {1, 2}
--   1 - 'a' -> {1}
--   1 - 'b' -> {2}
--   2 - 'a' -> {0, 2}

statesC :: States Int
statesC  = Set.fromList [0, 1, 2]

alphabetC :: Alphabet Char
alphabetC = Set.fromList "ab"

transC :: NFATransition Int Char
transC = Map.fromList
    [ ((0, Nothing),  Set.fromList [1, 2])
    , ((1, Just 'a'), Set.singleton 1)
    , ((1, Just 'b'), Set.singleton 2)
    , ((2, Just 'a'), Set.fromList [0, 2])
    ]

initialC :: Int
initialC = 0

finalsC :: States Int
finalsC = Set.fromList [2]

testComplexValid :: Test
testComplexValid = TestCase $
    assertBool "makeNFA should succeed for a complex NFA"
        (isJust (makeNFA statesC alphabetC transC initialC finalsC))

-- introduce an invalid mapping outside of statesC
transCbad :: NFATransition Int Char
transCbad = Map.insert (1, Just 'b') (Set.fromList [3]) transC

testComplexInvalidTrans :: Test
testComplexInvalidTrans = TestCase $
    assertBool "complex NFA invalid due to outside transition"
        (isNothing (makeNFA statesC alphabetC transCbad initialC finalsC))
