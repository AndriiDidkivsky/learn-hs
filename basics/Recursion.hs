module Recursion 
where

import Data.Char 
import Prelude hiding(reverse)

repeatN :: Int -> a -> [a]
repeatN 0 x = []
repeatN n x = x : repeatN (n - 1) x

suffixes :: String -> [String]
suffixes "" = []
suffixes s = s : suffixes (tail s)

allToUpper :: String -> String
allToUpper [] = []
allToUpper (chr : restString) = toUpper chr : allToUpper restString

snoc :: a -> [a] -> [a]
x `snoc` xs = xs ++ [x]

reverse :: [a] -> [a]
reverse [] = []
-- reverse (x:xs) = x `snoc` reverse xs  
reverse (x:xs) = reverse xs ++ [x]