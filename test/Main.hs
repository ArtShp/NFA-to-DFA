module Main (main) where

import Test.HUnit
import qualified System.Exit as Exit

import DFA.DFATests as DFATests

tests :: Test
tests = TestList (dfaTests)

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
