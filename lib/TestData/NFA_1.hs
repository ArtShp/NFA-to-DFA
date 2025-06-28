module TestData.NFA_1 (
    nfa_1
  , nfaTrans_1
  , nfaInputs_1
  , nfaOutputs_1
  , nfaIO_1
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified NFA

-- ==================== BEGIN - NFA 1 ====================

nfaTrans_1 :: NFA.NFATransition Int Char
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

nfa_1 :: NFA.NFA Int Char
nfa_1 = NFA.NFA
    { NFA.states       = Set.fromList [0 .. 9]
    , NFA.alphabet     = Set.fromList "ab"
    , NFA.transition   = nfaTrans_1
    , NFA.initialState = 0
    , NFA.finalStates  = Set.singleton 9
    }

nfaInputs_1 :: [String]
nfaInputs_1 = ["", "a", "aa", "aaaa"]

nfaOutputs_1 :: [Bool]
nfaOutputs_1 = [False, False, True, False]

nfaIO_1 :: [(String, Bool)]
nfaIO_1 = zip nfaInputs_1 nfaOutputs_1

-- ==================== END - NFA 1 ====================
