module NFA where

import AutomataBase

-- We use `Nothing` here to represent an ε-move.
type NFATransition q a = q -> Maybe a -> States q

data (Show q, Show a) => NFA q a = NFA
    { states       :: States q
    , alphabet     :: Alphabet a
    , transition   :: NFATransition q a
    , initialState :: q
    , finalStates  :: States q
    }
