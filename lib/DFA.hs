module DFA where

import AutomataBase

type DFATransition q a = q -> a -> Maybe q

data (Show q, Show a) => DFA q a = DFA
    { states       :: States q
    , alphabet     :: Alphabet a
    , transition   :: DFATransition q a
    , initialState :: q
    , finalStates  :: States q
    }
