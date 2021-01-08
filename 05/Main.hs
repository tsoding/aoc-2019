module Main where

import Intcode
import Text.Printf

solve :: Machine -> Int -> Int
solve machine ident = head $ getOutput $ execute $ setInput [ident] machine

main :: IO ()
main = do
  machine <- machineFromFile "./input.txt"
  printf "Part 1: %d\n" $ solve machine 1
  printf "Part 2: %d\n" $ solve machine 5
