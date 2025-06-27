module NFA.NFATests (nfaTests) where

import Test.HUnit

import NFA.NFAValidationTests
import NFA.NFASimulatorsTests

nfaTests :: [Test]
nfaTests = nfaValidationTests ++ nfaSimulatorsTests
