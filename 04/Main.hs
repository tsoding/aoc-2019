module Main where

import Data.List
import Text.Printf

isValid1 :: Int -> Bool
isValid1 x = isIncreasing && containsDouble
  where
    s = show x
    isIncreasing = s == sort s
    containsDouble = length (nub s) < length s

isValid2 :: Int -> Bool
isValid2 x = isIncreasing && containsDoubleGroup
  where
    s = show x
    isIncreasing = s == sort s
    containsDoubleGroup = not $ null $ filter (== 2) $ map length $ group s

solve :: (Int, Int) -> (Int -> Bool) -> Int
solve (low, high) isValid = length $ filter isValid [low .. high]

main :: IO ()
main = do
  let input = (245182, 790572)
  printf "Part 1: %d\n" $ solve input isValid1
  printf "Part 2: %d\n" $ solve input isValid2
