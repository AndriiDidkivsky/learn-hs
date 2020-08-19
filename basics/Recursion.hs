module Recursion where

import Data.Char 
import Prelude hiding(reverse, length, enumFromTo)


repeatN :: Int -> a -> [a]
repeatN 0 x = []
repeatN n x = x : repeatN (n - 1) x

suffixes :: String -> [String]
suffixes "" = []
suffixes s = s : suffixes (tail s)

allToUpper :: String -> String
allToUpper [] = []
allToUpper (chr : restString) = toUpper chr : allToUpper restString

-- snoc :: a -> [a] -> [a]
-- x `snoc` xs = xs ++ [x]
-- reverse (x:xs) = x `snoc` reverse xs  


-- O(n^2)
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x] 

-- O(n)
fastReverse :: [a] -> [a]
fastReverse xs = reverseAcc [] xs
  where
    reverseAcc :: [a] -> [a] -> [a]
    reverseAcc accList []     = accList
    reverseAcc accList (x:xs) = reverseAcc (x : accList) xs


-- O(n)
sum :: Num a => [a] -> a
sum xs = sumAcc 0 xs
  where
    sumAcc :: Num a => a -> [a] -> a
    sumAcc acc []       = acc
    sumAcc acc (x : xs) = sumAcc (x + acc) xs

type Point = (Int, Int)

distance (x1, y1) (x2, y2) 
   = sqrt (fromIntegral(dx * dx + dy * dy))
  where
    dx = x1 - x2
    dy = y1 - y2

closestPoint :: Point -> [Point] -> Point
closestPoint point [p] = p 
closestPoint point (p:ps) = 
  closestOfTwoPoints point p (closestPoint point ps)

closestOfTwoPoints :: Point -> Point -> Point -> Point
closestOfTwoPoints point p1 p2
  | distance point p1 < distance point p2 = p1
  | otherwise = p2

-- Exercices:

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length(xs)

-- 1:[2,3,4]           = [1,2,3,4]
-- 1:2:3:4:[]          = [1,2,3,4]
-- [1,2,3]:[4..7]      = Error because [Int]:[Int]
-- [1,2,3] ++ [4..7]   = [1,2,3,4,5,6,7]
-- 1:['a','b']         = Error because Int:[Char]
-- "abc"++"cd"         = "abccd"
-- "a":"bCc"           = Error because [Char]:[Char]
-- "a" ++ "bCc"        = "abCc"
-- 'a':'b'             = Error because Char:Char
-- 'a':"b"             = "ab"
-- [1,4,7] ++ 4:[5:[]] = [1,4,7,4,[5]] because type of the second list is incorect. [Int, [Int]] can't exists
-- [True,True:[]]      = Error see above
-- True:[True,False]   = [True, True, False]

fact :: Int -> Int
fact 0 = 1
fact n 
  | n > 0 = n * fact (n - 1)
  | otherwise = error "n should can't be less then zero"

enumFromTo :: Int -> Int -> [Int]
enumFromTo m n 
  | m <=n = enumFromToAcc [] m n
  | otherwise = error "m should be <= n"
  where
    enumFromToAcc :: [Int] -> Int -> Int -> [Int]
    enumFromToAcc acc m n
      | m == n = (m : acc)
      | otherwise = m : enumFromToAcc acc (m + 1) n

countOdds :: [Int] -> Int
countOdds xs = countOddsAcc 0 xs
  where 
    countOddsAcc :: Int -> [Int] -> Int
    countOddsAcc a [] = a 
    countOddsAcc acc (x:xs)
      | odd x = countOddsAcc (acc + 1) xs
      | otherwise = countOddsAcc acc xs


removeOdd :: [Int] -> [Int]
removeOdd xs = removeOddAcc [] xs
  where 
    removeOddAcc :: [Int] -> [Int] -> [Int]
    removeOddAcc acc [] = acc
    removeOddAcc acc (x:xs)
      | odd x = removeOddAcc acc xs
      | otherwise = removeOddAcc (x: acc) xs