module TestData.DFA_1 (
    dfa_1
  , dfaIO_1
  , dfaInputs_1
  , dfaOutputs_1
) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import DFA

-- ==================== BEGIN - DFA 1 ====================

dfa_1 :: DFA Int Char
dfa_1 = DFA
    { dfaStates       = Set.fromList [0, 1]
    , dfaAlphabet     = Set.fromList "ab"
    , dfaTransition   = Map.fromList
        [ ((0, 'a'), 1)
        , ((1, 'a'), 0)
        , ((1, 'b'), 1)
        , ((0, 'b'), 0)
        ]
    , dfaInitialState = 0
    , dfaFinalStates  = Set.singleton 0
    }



dfaIO_1 :: [(String, Bool)]
dfaIO_1 = 
    [ ("", True)
    , ("a", False)
    , ("aba", True)
    , ("baaa", False)
    , ("abbaaab", True)
    ]

dfaInputs_1 :: [String]
dfaInputs_1 = fst $ unzip dfaIO_1

dfaOutputs_1 :: [Bool]
dfaOutputs_1 = snd $ unzip dfaIO_1

-- ==================== END - DFA 1 ====================
