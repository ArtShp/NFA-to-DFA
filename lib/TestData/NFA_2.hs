module TestData.NFA_2 (
    nfa_2
  , nfaTrans_2
  , nfaInputs_2
  , nfaOutputs_2
  , nfaIO_2
  , dfaExpected_2
  , dfaExpectedTrans_2
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AutomataBase
import qualified DFA
import qualified NFA

-- ==================== BEGIN - NFA 2 ====================

nfa_2 :: NFA.NFA Int Char
nfa_2 = NFA.NFA
    { NFA.states       = Set.fromList [0 .. 5]
    , NFA.alphabet     = Set.fromList "0123456789+-."
    , NFA.transition   = nfaTrans_2
    , NFA.initialState = 0
    , NFA.finalStates  = Set.singleton 5
    }

nfaTrans_2 :: NFA.NFATransition Int Char
nfaTrans_2 = Map.fromList (
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



nfaInputs_2 :: [String]
nfaInputs_2 = ["12.34", "-1.", "+.555", ".", "13249", "--12.12"]

nfaOutputs_2 :: [Bool]
nfaOutputs_2 = [True, True, True, False, False, False]

nfaIO_2 :: [(String, Bool)]
nfaIO_2 = zip nfaInputs_2 nfaOutputs_2



dfaExpected_2 :: DFA.DFA (States Int) Char
dfaExpected_2 = DFA.DFA
    { DFA.states       = Set.fromList [Set.fromList [0,1], Set.fromList [1], Set.fromList [1,4], Set.fromList [2], Set.fromList [2,3,5], Set.fromList [3,5]]
    , DFA.alphabet     = Set.fromList "0123456789+-."
    , DFA.transition   = dfaExpectedTrans_2
    , DFA.initialState = Set.fromList [0,1]
    , DFA.finalStates  = Set.fromList [Set.fromList [2,3,5], Set.fromList [3,5]]
    }

dfaExpectedTrans_2 :: DFA.DFATransition (States Int) Char
dfaExpectedTrans_2 = Map.fromList $ concat
    [ trans1 [0,1] [1,4] "0123456789"
    , trans1 [0,1] [1] "+-"
    , trans1 [0,1] [2] "."
    , trans1 [1] [2] "."
    , trans1 [1] [1,4] "0123456789"
    , trans1 [1,4] [2,3,5] "."
    , trans1 [1,4] [1,4] "0123456789"
    , trans1 [2] [3,5] "0123456789"
    , trans1 [2,3,5] [3,5] "0123456789"
    , trans1 [3,5] [3,5] "0123456789"
    ]

-- ==================== END - NFA 2 ====================

-- ==================== MISC ====================

transDigit :: Int -> [Int] -> [((Int, Maybe Char), States Int)]
transDigit from tos = 
    [((from, Just c), Set.fromList tos) | c <- ['0' .. '9']]

trans1 :: [Int] -> [Int] -> [Char] -> [((States Int, Char), States Int)]
trans1 xs ys as =
    [((Set.fromList xs, c), Set.fromList ys) | c <- as]
