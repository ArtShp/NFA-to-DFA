module AutomataBase where

import qualified Data.Set as Set

-- #TODO: Maybe it's better to use 
-- newtype States q = States [q], or even data
type States q = Set.Set q

-- #TODO: Maybe it's better to use 
-- newtype Alphabet a = Alphabet [a], or even data
type Alphabet a = Set.Set a
