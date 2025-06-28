module DFA.DFASimulatorsTests (dfaSimulatorsTests) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Test.HUnit
import Data.Maybe (isNothing)

import Automata

dfaSimulatorsTests :: [Test]
dfaSimulatorsTests =
    [ TestLabel "one step"              testOneStep
    , TestLabel "simulate from state"   testFromState
    , TestLabel "simulate fail"         testFromStateFail
    , TestLabel "with history"          testHistory
    , TestLabel "from start"            testFromStart
    , TestLabel "from start history"    testFromStartHistory
    , TestLabel "is accepted"           testIsAccepted
    , TestLabel "is rejected"           testIsRejected
    ]

-- A simple 2-state toggle DFA: 'a' flips 0 and 1, 'b' is self-loop
states_ :: States Int
states_ = Set.fromList [0, 1]

alphabet_ :: Alphabet Char
alphabet_ = Set.fromList "ab"

toggleTrans :: DFATransition Int Char
toggleTrans = Map.fromList
    [ ((0, 'a'), 1), ((1, 'a'), 0)
    , ((0, 'b'), 0), ((1, 'b'), 1)
    ]

toggleDFA :: DFA Int Char
toggleDFA = DFA states_ alphabet_ toggleTrans 0 (Set.fromList [1])

-- Tests

testOneStep :: Test
testOneStep = TestCase $
    assertEqual "one step on 'a' from 0" (Just 1)
        (dfaSimulateOneStep toggleDFA 0 'a')

testFromState :: Test
testFromState = TestCase $
    assertEqual "simulate \"aa\" from 0" (Just 0)
        (dfaSimulateFromState toggleDFA 0 "aa")

testFromStateFail :: Test
testFromStateFail = TestCase $
    assertBool "unknown symbol -> Nothing"
        (isNothing (dfaSimulateFromState toggleDFA 0 "c"))

testHistory :: Test
testHistory = TestCase $
    assertEqual "history for \"ab\"" ([(0, 'a'), (1, 'b')], Just 1)
        (dfaSimulateFromStateWithHistory toggleDFA 0 "ab")

testFromStart :: Test
testFromStart = TestCase $
    assertEqual "fromStart \"a\"" (Just 1)
        (dfaSimulateFromStart toggleDFA "a")

testFromStartHistory :: Test
testFromStartHistory = TestCase $
    assertEqual "fromStartWithHistory \"a\"" ([(0, 'a')], Just 1)
        (dfaSimulateFromStartWithHistory toggleDFA "a")

testIsAccepted :: Test
testIsAccepted = TestCase $
    assertBool "\"a\" accepted"
        (dfaIsAccepted toggleDFA "a")

testIsRejected :: Test
testIsRejected = TestCase $
    assertBool "\"aabaa\" rejected"
        (not (dfaIsAccepted toggleDFA "aabaa"))
