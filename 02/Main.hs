{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Array
import Text.Printf
import Data.List

import Intcode

input :: Int -> Int -> Machine -> Machine
input noun verb machine@(Machine memory _ _) =
  machine {getMemory = memory // [(1, noun), (2, verb)]}

output :: Int -> Int -> Machine -> Int
output noun verb = (! 0) . getMemory . execute . input noun verb

part1 :: Machine -> Int
part1 = output 12 2

part2 :: Machine -> Int
part2 machine =
  head $
  [ 100 * noun + verb
  | noun <- [0 .. 99]
  , verb <- [0 .. 99]
  , output noun verb machine == 19690720
  ]

main :: IO ()
main = do
  machine <- machineFromFile "./input.txt"
  printf "Part 1: %d\n" $ part1 machine
  printf "Part 2: %d\n" $ part2 machine
