module Main where

import Intcode
import Text.Printf

main :: IO ()
main = do
  machine <- machineFromFile "input.txt"
  printf "Part 1: %d\n" $ head $ getOutput $ execute $ pushInput 1 machine
  printf "Part 2: %d\n" $ head $ getOutput $ execute $ pushInput 2 machine
