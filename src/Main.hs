module Main where

import Control.Monad ( forM_ )
import qualified Data.Map as Map
import qualified Data.Set as Set

import NFA
import Convertions.NFAConvertions
import Simulators.NFASimulators
import Simulators.DFASimulators

main :: IO ()
main = do
    putStrLn "=== NFA to DFA Conversion and Simulation ===\n"

    -- Define a NFA:
    let transMix = Map.fromList
            [ ((0, Just 'a'), Set.singleton 1)
            , ((1, Nothing), Set.fromList [2, 4])
            , ((2, Just 'a'), Set.singleton 3)
            , ((3, Nothing), Set.singleton 9)
            , ((4, Just 'b'), Set.singleton 5)
            , ((5, Nothing), Set.fromList [6, 8, 9])
            , ((6, Just 'b'), Set.singleton 7)
            , ((7, Nothing), Set.fromList [6, 8, 9])
            , ((8, Nothing), Set.singleton 9)
            ]

        nfaMix :: NFA Int Char
        nfaMix = NFA
            { states       = Set.fromList [0 .. 9]
            , alphabet     = Set.fromList "ab"
            , transition   = transMix
            , initialState = 0
            , finalStates  = Set.singleton 9
            }

    putStrLn "Defined NFA:"
    print nfaMix

    -- Compute its ε-closure of the start state
    let startClosure = epsilonClosure transMix (Set.singleton 0)
    putStrLn "\nε-closure of {0}:"
    print startClosure

    -- Convert to DFA
    let dfa = nfaToDfa nfaMix
    putStrLn "\nConverted DFA:"
    print dfa

    -- Try a few input strings on the DFA
    let inputsMix = ["", "a", "aa", "aaaa"]
    putStrLn "\n-- Testing DFA acceptance --"
    forM_ inputsMix $ \w -> do
        putStrLn $ "Input: \"" ++ w ++ "\"  =>  " ++
                   (if dfaIsAccepted dfa w then "accepted" else "rejected")

    -- And also check NFA acceptance
    putStrLn "\n-- Testing NFA acceptance --"
    forM_ inputsMix $ \w -> do
        putStrLn $ "Input: \"" ++ w ++ "\"  =>  " ++
                   (if nfaIsAccepted nfaMix w then "accepted" else "rejected")

    putStrLn "\n=== Complex NFA to DFA Conversion and Simulation ===\n"

    -- Define a NFA:
    let transComplex = Map.fromList (
            [ ((0, Just '+'), Set.singleton 1)
            , ((0, Just '-'), Set.singleton 1)
            , ((0, Nothing), Set.singleton 1)
            , ((1, Just '.'), Set.singleton 2)
            , ((4, Just '.'), Set.singleton 3)
            , ((3, Nothing), Set.singleton 5)
            ] ++
            transDigit 1 [1, 4] ++
            transDigit 2 [3] ++
            transDigit 3 [3])

        nfaComplex :: NFA Int Char
        nfaComplex = NFA
            { states       = Set.fromList [0 .. 5]
            , alphabet     = Set.fromList "0123456789+-."
            , transition   = transComplex
            , initialState = 0
            , finalStates  = Set.singleton 5
            }
        
        transDigit from tos = 
            [((from, Just c), Set.fromList tos) | c <- ['0' .. '9']]

    putStrLn "Defined NFA:"
    print nfaComplex

    -- Compute its ε-closure of the start state
    let startClosureComplex = epsilonClosure transComplex (Set.singleton 0)
    putStrLn "\nε-closure of {0}:"
    print startClosureComplex

    -- Convert to DFA
    let dfaComplex = nfaToDfa nfaComplex
    putStrLn "\nConverted DFA:"
    print dfaComplex

    -- Try a few input strings on the DFA
    let inputsComplex = ["12.34", "-1.", "+.555", ".", "13249", "--12.12"]
    putStrLn "\n-- Testing DFA acceptance --"
    forM_ inputsComplex $ \w -> do
        putStrLn $ "Input: \"" ++ w ++ "\"  =>  " ++
                   (if dfaIsAccepted dfaComplex w then "accepted" else "rejected")

    -- And also check NFA acceptance
    putStrLn "\n-- Testing NFA acceptance --"
    forM_ inputsComplex $ \w -> do
        putStrLn $ "Input: \"" ++ w ++ "\"  =>  " ++
                   (if nfaIsAccepted nfaComplex w then "accepted" else "rejected")

    putStrLn "\n=== Simulating NFA and DFA with History ===\n"

    let inputWithHistory = "197.30"
    forM_ [dfaSimulateFromStartWithHistory dfaComplex inputWithHistory] $ \(history, finalState) ->
        putStrLn $ "DFA history for input \"" ++ inputWithHistory ++ "\":\n" ++
                   show history ++ "\nFinal state: " ++ show finalState

    forM_ (nfaSimulateFromStartWithHistory nfaComplex inputWithHistory) $ \(history, finalStates_) ->
        putStrLn $ "\nNFA history for input \"" ++ inputWithHistory ++ "\":\n" ++
                   show history ++ "\nFinal states: " ++ show finalStates_
