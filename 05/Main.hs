module Main where

import Intcode
import Text.Printf

part1 :: Machine -> Int
part1 = head . getOutput . execute . setInput [1]

main :: IO ()
main = do
  machine <- machineFromFile "./input.txt"
  printf "Part 1: %d\n" $ part1 machine
