module DFA.DFAValidationTests (dfaValidationTests) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Test.HUnit
import Data.Maybe (isJust, isNothing)
import DFA

-- Common pieces for tests:
states_ :: Set.Set Int
states_ = Set.fromList [0,1]

alphabet_ :: Set.Set Char
alphabet_ = Set.fromList "ab"

validTrans :: DFATransition Int Char
validTrans = Map.fromList
    [((s, c), s)
    | s <- Set.toList states_, c <- Set.toList alphabet_
    ]

initial :: Int
initial = 0

finals :: Set.Set Int
finals = Set.fromList [1]

dfaValidationTests :: [Test]
dfaValidationTests =
    [ TestLabel "valid simple DFA"           testValidDFA
    , TestLabel "empty states"               testEmptyStates
    , TestLabel "empty alphabet"             testEmptyAlphabet
    , TestLabel "initial not in states"      testInitialNotInStates
    , TestLabel "final not subset of states" testFinalNotSubset
    ]

testValidDFA :: Test
testValidDFA = TestCase $
    assertBool "makeDFA should succeed for a simple valid DFA"
        (isJust (makeDFA states_ alphabet_ validTrans initial finals))

testEmptyStates :: Test
testEmptyStates = TestCase $
    assertBool "makeDFA should fail on empty states"
        (isNothing (makeDFA Set.empty alphabet_ validTrans initial finals))

testEmptyAlphabet :: Test
testEmptyAlphabet = TestCase $
    assertBool "makeDFA should fail on empty alphabet"
        (isNothing (makeDFA states_ Set.empty validTrans initial finals))

testInitialNotInStates :: Test
testInitialNotInStates = TestCase $
    assertBool "makeDFA should fail if initialState is not in states"
        (isNothing (makeDFA states_ alphabet_ validTrans 42 finals))

testFinalNotSubset :: Test
testFinalNotSubset = TestCase $
    assertBool "makeDFA should fail if finalStates is not a subset of states"
        (isNothing (makeDFA states_ alphabet_ validTrans initial (Set.fromList [2])))
