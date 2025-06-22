module DFA.DFATests (dfaTests) where

import Test.HUnit

import DFA.DFAValidationTests

dfaTests :: [Test]
dfaTests = dfaValidationTests
