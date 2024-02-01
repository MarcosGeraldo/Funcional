{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module P2 where

--import Data.Char
--import Test.Tasty
--import Test.Tasty.HUnit as TH
--import Test.Tasty.QuickCheck as TQ
--import Test.QuickCheck
--import Test

import Data.Char


import System.IO

data Vec3 = Vec3 Int Int Int

instance Eq Vec3 where
    (Vec3 x y z) == (Vec3 x' y' z') = x == x' && y == y' && z == z'

check :: Vec3 -> Vec3 -> Bool
check x y = x == y

data Person = Person {name :: String, age :: Int}

instance Eq Person where
    Person x _ == Person x' _ = x == x'
    x/=y = not (x==y)

checkP :: Person -> Person -> Bool
checkP x y = x == y

--instance Show Person  where
--    show Person y = show x 

instance Show Person where
    show (Person x y) = show x 

---------


inRange :: Int -> Int -> [Int] -> [Int]
inRange _ _ [] = []
inRange x y (z:zs)
  |y <= 1 = []
  |x <= 1 = z :  inRange (x-1) (y-1) zs
  |otherwise = inRange (x-1) y zs

comp :: [Int] -> [Int] -> Bool
comp [] [] = True
comp [] _ = False
comp _ [] = False
comp (x:xs) (y:ys) = x==y && comp xs ys

inRangeProperty :: Int -> Int -> [Int] -> Bool
inRangeProperty x y z
    |(inRange x y z) == [] && x==0 && y==0 && z==[] = True
    |(length z) < (length (inRange x y z)) = False
    |((length z) == (length (inRange x y z))) && (comp (inRange x y z) z) = True
    |((length (inRange x y z)) < (length z)) && ((length (inRange x y z)) == (y-x+1)) = True
    |otherwise = (comp (inRange x y z) z) 


--inRangeUnit :: TestTree
--inRangeUnit = testCase "inRage test" $ inRange 2 5 [1..10] @?= [2,3,4,5]

----------
conc :: [Int] -> [Int] -> [Int]
conc [] y = y
conc x [] = x
conc (x:xs) (y:ys) 
 |y<x = y : conc (x:xs) ys
 |y>x = x : conc xs (y:ys)
 |otherwise = x : conc xs ys 

createList :: Int -> [Int]
createList x = conc (conc[x] [2*x]) (conc ([3*x]) (createList (x+1)))

createList2 :: Int -> [Int]
createList2 x | x<1000 = conc (conc[x] [2*x]) (conc ([3*x]) (createList2 (x+1)))
              | otherwise = []

pathFile :: FilePath
pathFile = "words.txt"

est :: IO ()
est = do
        content <- openFile pathFile ReadMode
        palavras <- hGetContents content
        putStrLn palavras
        putStrLn ("Numero de Palavras: " ++ show(worldCount(words palavras)))
        putStrLn ("Numero de Linhas: " ++ show(lineCount palavras))


worldCount :: [String] -> Int
worldCount [] = 0
worldCount (x:xs) = 1 + worldCount xs

lineCount :: String -> Int
lineCount [] = 1
lineCount (y:ys) | y =='\n' = 1 + lineCount ys
                      | otherwise = lineCount ys


helloThere :: IO ()
helloThere = do
  putStrLn "Hello, what is your name?"
  name <- getLine
  putStrLn ("Hello there " ++ name)

--------------

--kind= * -> * -> *
data Toy a b = Output a b | Bell b | Done
data Maybe2 a = Just2 a | Nothing2

instance Functor Maybe2 where
    fmap _ Nothing2 = Nothing2
    fmap f (Just2 x) = Just2 (f x)

--instance Functor Toy where
--    fmap _ Done = Done
--    fmap f (Bell b) = (Bell (f b))
--    fmap f (Output a b) = (Output (f a) (f b))

------------------

newtype Parser s a = Parser { runParser :: [s] -> [(a,[s])] }

token :: Eq s => [s] -> Parser s [s]
token s = Parser (\ inp ->  if s == (take n inp)
                            then [(s, drop n inp)]
                            else [])
                            where
                                n = length s

takeId :: Parser s [s]
takeId = Parser (\ inp -> [((takeIdExcept inp),
                            drop (length (takeIdExcept inp)) inp)])


takeIdExcept :: String -> String
takeIdExcept []=[]
takeIdExcept (x:xs) |x == ',' = []
                    |otherwise = x : takeIdExcept xs