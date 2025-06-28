module TestData.NFA_2 (
    nfa_2
  , nfaTrans_2
  , nfaInputs_2
  , nfaOutputs_2
  , nfaIO_2
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AutomataBase (States)
import qualified NFA

-- ==================== BEGIN - NFA 2 ====================

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

nfa_2 :: NFA.NFA Int Char
nfa_2 = NFA.NFA
    { NFA.states       = Set.fromList [0 .. 5]
    , NFA.alphabet     = Set.fromList "0123456789+-."
    , NFA.transition   = nfaTrans_2
    , NFA.initialState = 0
    , NFA.finalStates  = Set.singleton 5
    }

transDigit :: Int -> [Int] -> [((Int, Maybe Char), States Int)]
transDigit from tos = 
    [((from, Just c), Set.fromList tos) | c <- ['0' .. '9']]

nfaInputs_2 :: [String]
nfaInputs_2 = ["12.34", "-1.", "+.555", ".", "13249", "--12.12"]

nfaOutputs_2 :: [Bool]
nfaOutputs_2 = [True, True, True, False, False, False]

nfaIO_2 :: [(String, Bool)]
nfaIO_2 = zip nfaInputs_2 nfaOutputs_2

-- ==================== END - NFA 2 ====================
