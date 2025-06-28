module Main where

import Control.Monad ( forM_ )
import qualified Data.Set as Set

import Automata
import qualified AutomataTestData as TD

main :: IO ()
main = do
    putStrLn "=== NFA to DFA Conversion and Simulation ===\n"

    putStrLn "Defined NFA:"
    print TD.nfa_1

    -- Compute its ε-closure of the start state
    let startClosure_1 = epsilonClosure TD.nfaTrans_1 (Set.singleton 0)
    putStrLn "\nε-closure of {0}:"
    print startClosure_1

    -- Convert to DFA
    let dfa_1 = nfaToDfa TD.nfa_1
    putStrLn "\nConverted DFA:"
    print dfa_1

    -- Try a few input strings on the DFA
    putStrLn "\n-- Testing DFA acceptance --"
    forM_ TD.nfaInputs_1 $ \w -> do
        putStrLn $ "Input: \"" ++ w ++ "\"  =>  " ++
                   (if dfaIsAccepted dfa_1 w then "accepted" else "rejected")

    -- And also check NFA acceptance
    putStrLn "\n-- Testing NFA acceptance --"
    forM_ TD.nfaInputs_1 $ \w -> do
        putStrLn $ "Input: \"" ++ w ++ "\"  =>  " ++
                   (if nfaIsAccepted TD.nfa_1 w then "accepted" else "rejected")





    putStrLn "\n=== Complex NFA to DFA Conversion and Simulation ===\n"

    putStrLn "Defined NFA:"
    print TD.nfa_2

    -- Compute its ε-closure of the start state
    let startClosure_2 = epsilonClosure TD.nfaTrans_2 (Set.singleton 0)
    putStrLn "\nε-closure of {0}:"
    print startClosure_2

    -- Convert to DFA
    let dfa_2 = nfaToDfa TD.nfa_2
    putStrLn "\nConverted DFA:"
    print dfa_2

    -- Try a few input strings on the DFA
    putStrLn "\n-- Testing DFA acceptance --"
    forM_ TD.nfaInputs_2 $ \w -> do
        putStrLn $ "Input: \"" ++ w ++ "\"  =>  " ++
                   (if dfaIsAccepted dfa_2 w then "accepted" else "rejected")

    -- And also check NFA acceptance
    putStrLn "\n-- Testing NFA acceptance --"
    forM_ TD.nfaInputs_2 $ \w -> do
        putStrLn $ "Input: \"" ++ w ++ "\"  =>  " ++
                   (if nfaIsAccepted TD.nfa_2 w then "accepted" else "rejected")





    putStrLn "\n=== Simulating NFA and DFA with History ===\n"

    let inputWithHistory = "197.30"
    forM_ [dfaSimulateFromStartWithHistory dfa_2 inputWithHistory] $ \(history, finalState) ->
        putStrLn $ "DFA history for input \"" ++ inputWithHistory ++ "\":\n" ++
                   show history ++ "\nFinal state: " ++ show finalState

    forM_ (nfaSimulateFromStartWithHistory TD.nfa_2 inputWithHistory) $ \(history, finalStates_) ->
        putStrLn $ "\nNFA history for input \"" ++ inputWithHistory ++ "\":\n" ++
                   show history ++ "\nFinal states: " ++ show finalStates_
