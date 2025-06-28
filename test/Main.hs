module Main (main) where

import Test.HUnit
import qualified System.Exit as Exit

import DFA.DFATests as DFATests
import NFA.NFATests as NFATests

tests :: Test
tests = TestList (dfaTests ++ nfaTests)

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
