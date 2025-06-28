module AutomataBase where

import qualified Data.Set as Set

type States q = Set.Set q

type Alphabet a = Set.Set a

showStringListNoBrackets :: [String] -> String
showStringListNoBrackets arr = go arr
    where
        go [] = ""
        go [x] = x
        go (x:xs) = x ++ ", " ++ go xs

showListNoBrackets :: (Ord a, Show a) => [a] -> String
showListNoBrackets arr = go arr
    where
        go [] = ""
        go [x] = show x
        go (x:xs) = show x ++ ", " ++ go xs

showStates :: (Ord q, Show q) => States q -> String
showStates states = "{" ++ (showListNoBrackets (Set.toList states)) ++ "}"

showAlphabet :: (Ord a, Show a) => Alphabet a -> String
showAlphabet alphabet = "{" ++ (showListNoBrackets (Set.toList alphabet)) ++ "}"
