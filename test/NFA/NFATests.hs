module NFA.NFATests (nfaTests) where

import Test.HUnit

import NFA.NFAValidationTests
import NFA.NFASimulatorsTests
import NFA.NFAConvertionsTests

nfaTests :: [Test]
nfaTests = nfaValidationTests ++ nfaSimulatorsTests ++ nfaConvertionsTests
