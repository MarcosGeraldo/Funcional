import Distribution.Simple.Utils (xargs)
import Distribution.SPDX (LicenseId(CDDL_1_1))
--import Prelude hiding ( map, filter, foldr, foldl, flip, (.))
import Data.Char
xor :: Bool -> Bool -> Bool
xor False False = False
xor False True = True
xor True False = True 
xor True True = False


existsPositive :: [Int]->Bool
existsPositive [] = False
existsPositive (x : xs) = (x<0) || existsPositive xs

--Aula 3
bools :: [Bool]
bools = [True,True,False]

nums :: [[Int]]
nums = [[1,2,3],[5,4,5],[5,8]]

add :: Int -> Int -> Int -> Int
add x y z = x+y+z

copy :: a-> (a,a)
copy x = (x,x)

apply :: (a->b) -> a -> b
apply func x = func x

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

--Aula4

minList :: [Int]->Int
minList (x : xs) = minAux x xs
 where
  minAux x [] = x
  minAux y (x:xs)
   | y < x = minAux y xs
   | otherwise = minAux x xs

andList :: [Bool] -> Bool
andList (x:xs) = x && andList xs

orList :: [Bool] -> Bool
orList (x:xs) = x || orList xs

indexOf :: Int -> [Int] -> Int
indexOf y (x:xs) = indexOfAux y (x:xs) 0
 where
  indexOfAux _ [] _ = -1
  indexOfAux y (x:xs) z 
   |(x/=y) = indexOfAux y xs (z+1)
   |otherwise = z


removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll y (x:xs)
 |(x==y) = removeAll y xs
 |otherwise = x : removeAll y xs


--Aula05

capitalize :: String -> String
capitalize xs = map toUpper xs

takeWhile ::  (a -> Bool) -> [a] -> [a]
takeWhile f = foldr step []
 where
    step x ac = f x : ac


all1 :: (a -> Bool) -> [a] -> Bool
all1 f (x:xs) =  f x && (all1 f xs)

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr step []
   where
      step x ac = f x && ac

data Point = Point Float Float

data Shape = Rectangle Point Float Float | Circle Point Float | Triangle Point Point Point




