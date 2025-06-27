module DFA.DFATests (dfaTests) where

import Test.HUnit

import DFA.DFAValidationTests
import DFA.DFASimulatorsTests

dfaTests :: [Test]
dfaTests = dfaValidationTests ++ dfaSimulatorsTests
