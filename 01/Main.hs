module Main where

import Text.Printf

type Fuel = Int -> Int

fuel1 :: Fuel
fuel1 m = (m `div` 3) - 2

fuel2 :: Fuel
fuel2 m
  | n <= 0 = 0
  | otherwise = n + fuel2 n
  where
    n = (m `div` 3) - 2

solve :: Fuel -> [Int] -> Int
solve fuel = sum . map fuel

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input.txt"
  printf "Part 1: %d\n" $ solve fuel1 input
  printf "Part 2: %d\n" $ solve fuel2 input
