module DFA.DFAValidationTests (dfaValidationTests) where

import Test.HUnit
-- import Data.Maybe (isJust, isNothing)
-- import DFA

dfaValidationTests :: [Test]
dfaValidationTests = []
    -- [ TestLabel "valid simple DFA"             testValidDFA
    -- , TestLabel "empty states"                 testEmptyStates
    -- , TestLabel "empty alphabet"               testEmptyAlphabet
    -- , TestLabel "initial not in states"        testInitialNotInStates
    -- , TestLabel "final not a subset of states" testFinalNotSubset
    -- ]

-- simpleLoop :: Int -> Char -> Maybe Int
-- simpleLoop q _ = Just q

-- testValidDFA :: Test
-- testValidDFA = TestCase $
--     assertBool "makeDFA should succeed for a simple valid DFA"
--         (isJust (makeDFA [0, 1] "ab" simpleLoop 0 [1]))

-- testEmptyStates :: Test
-- testEmptyStates = TestCase $
--     assertBool "makeDFA should fail on empty states"
--         (isNothing (makeDFA [] "a" simpleLoop 0 []))

-- testEmptyAlphabet :: Test
-- testEmptyAlphabet = TestCase $
--     assertBool "makeDFA should fail on empty alphabet"
--         (isNothing (makeDFA [0] "" simpleLoop 0 [0]))

-- testInitialNotInStates :: Test
-- testInitialNotInStates = TestCase $
--     assertBool "makeDFA should fail if initialState is not in states"
--         (isNothing (makeDFA [1] "a" simpleLoop 0 [1]))

-- testFinalNotSubset :: Test
-- testFinalNotSubset = TestCase $
--     assertBool "makeDFA should fail if finalStates is not a subset of states"
--         (isNothing (makeDFA [0, 1, 2] "a" simpleLoop 0 [1, 3]))
