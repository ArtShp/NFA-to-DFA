module TestData.NFA_1 (
    nfa_1
  , nfaTrans_1
  , nfaIO_1
  , nfaInputs_1
  , nfaOutputs_1
  , dfaExpected_1
  , dfaExpectedTrans_1
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AutomataBase
import DFA
import NFA

-- ==================== BEGIN - NFA 1 ====================

nfa_1 :: NFA Int Char
nfa_1 = NFA
    { nfaStates       = Set.fromList [0 .. 9]
    , nfaAlphabet     = Set.fromList "ab"
    , nfaTransition   = nfaTrans_1
    , nfaInitialState = 0
    , nfaFinalStates  = Set.singleton 9
    }

nfaTrans_1 :: NFATransition Int Char
nfaTrans_1 = Map.fromList
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



nfaIO_1 :: [(String, Bool)]
nfaIO_1 =
    [ ("", False)
    , ("a", False)
    , ("aa", True)
    , ("aaaa", False)
    ]

nfaInputs_1 :: [String]
nfaInputs_1 = fst $ unzip nfaIO_1

nfaOutputs_1 :: [Bool]
nfaOutputs_1 = snd $ unzip nfaIO_1



dfaExpected_1 :: DFA (States Int) Char
dfaExpected_1 = DFA
    { dfaStates       = Set.fromList [Set.fromList [0], Set.fromList [1,2,4], Set.fromList [3,9], Set.fromList [5,6,8,9], Set.fromList [6,7,8,9]]
    , dfaAlphabet     = Set.fromList "ab"
    , dfaTransition   = dfaExpectedTrans_1
    , dfaInitialState = Set.singleton 0
    , dfaFinalStates  = Set.fromList [Set.fromList [3,9], Set.fromList [5,6,8,9], Set.fromList [6,7,8,9]]
    }

dfaExpectedTrans_1 :: DFATransition (States Int) Char
dfaExpectedTrans_1 = Map.fromList
    [ ((Set.fromList [0], 'a'), Set.fromList [1,2,4])
    , ((Set.fromList [1,2,4], 'a'), Set.fromList [3,9])
    , ((Set.fromList [1,2,4], 'b'), Set.fromList [5,6,8,9])
    , ((Set.fromList [5,6,8,9], 'b'), Set.fromList [6,7,8,9])
    , ((Set.fromList [6,7,8,9], 'b'), Set.fromList [6,7,8,9])
    ]

-- ==================== END - NFA 1 ====================
