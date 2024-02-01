module Lib
    ( someFunc
    ) where

--import Data.Char
--import Test.Tasty
--import Test.Tasty.HUnit as TH
--import Test.Tasty.QuickCheck as TQ
--import Test.QuickCheck
--import Test
someFunc :: IO ()
someFunc = putStrLn "Rodou"

--inRange :: Int -> Int -> [Int] -> [Int]
--inRange _ _ [] = []
--inRange x y (z:zs)
--  |y <= 1 = [] 
--  |x <= 1 = z :  inRange (x-1) (y-1) zs
---  |otherwise = inRange (x-1) y zs

--inRangeUnit :: TestTree
--inRangeUnit = testCase "inRage test" $ inRange 2 5 [1..10] @?= [2,3,4,5]
