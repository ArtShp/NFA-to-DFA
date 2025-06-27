module NFA.NFATests (nfaTests) where

import Test.HUnit

import NFA.NFAValidationTests

nfaTests :: [Test]
nfaTests = nfaValidationTests
