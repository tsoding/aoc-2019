{-# LANGUAGE OverloadedStrings #-}
module Intcode where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Array
import Text.Printf
import Data.List

type Address = Int

type Memory = Array Address Int

data Machine = Machine
  { getMemory :: !Memory
  , getIp :: !Int
  , isHalt :: !Bool
  , getInput :: [Int]
  , getOutput :: [Int]
  } deriving (Show)

emptyMachine :: Machine
emptyMachine = Machine (array (0, 0) [(0, 0)]) 0 False [] []

setInput :: [Int] -> Machine -> Machine
setInput input machine = machine { getInput = input }

setOutput :: [Int] -> Machine -> Machine
setOutput output machine = machine { getOutput = output }

setMemory :: Memory -> Machine -> Machine
setMemory memory machine = machine { getMemory = memory }

memoryFromFile :: FilePath -> IO Memory
memoryFromFile filePath = do
  xs <-
    map (read . T.unpack . T.strip) . T.splitOn "," <$> T.readFile filePath
  return $ memoryFromImage xs

machineFromFile :: FilePath -> IO Machine
machineFromFile filePath = machineFromMemory <$> memoryFromFile filePath

machineFromMemory :: Memory -> Machine
machineFromMemory memory = Machine memory 0 False [] []

memoryFromImage :: [Int] -> Memory
memoryFromImage xs = array (0, n - 1) (zip [0 ..] xs)
  where n = length xs


data Mode = Pos | Imm deriving Show
type Opcode = Int
type Param = Int

-- ABCDE
--  1002

decodeMode :: Int -> Mode
decodeMode 0 = Pos
decodeMode 1 = Imm
decodeMode x = error $ printf "Incorrect parameter mode %d" x

decodeOpcode :: Int -> (Opcode, Mode, Mode, Mode)
decodeOpcode x =
  ( x `mod` 100
  , decodeMode $ x `div` 100 `mod` 10
  , decodeMode $ x `div` 1000 `mod` 10
  , decodeMode $ x `div` 10000 `mod` 10)

obtainParam :: Memory -> Param -> Mode -> Int
obtainParam memory param Pos = memory ! param
obtainParam _ param Imm = param

step :: Machine -> Machine
step machine
  | isHalt machine = machine
  | otherwise =
    let ip = getIp machine
        memory = getMemory machine
        input = getInput machine
        output = getOutput machine
        op = memory ! ip
        (opcode, m1, m2, m3) = decodeOpcode op
     in case opcode of
          1 ->
            let a1 = obtainParam memory (memory ! (ip + 1)) m1
                a2 = obtainParam memory (memory ! (ip + 2)) m2
                dst = memory ! (ip + 3)
             in machine {getMemory = memory // [(dst, a1 + a2)], getIp = ip + 4}
          2 ->
            let a1 = obtainParam memory (memory ! (ip + 1)) m1
                a2 = obtainParam memory (memory ! (ip + 2)) m2
                dst = memory ! (ip + 3)
             in machine {getMemory = memory // [(dst, a1 * a2)], getIp = ip + 4}
          3 ->
            case input of
              (x:input') ->
                let dst = memory ! (ip + 1)
                 in machine
                      { getMemory = memory // [(dst, x)]
                      , getIp = ip + 2
                      , getInput = input'
                      }
              _ ->
                error $ printf "Empty input for read opcode at position %d" ip
          4 ->
            let a1 = obtainParam memory (memory ! (ip + 1)) m1
             in machine {getOutput = a1 : output, getIp = ip + 2}
          5 ->
            let a1 = obtainParam memory (memory ! (ip + 1)) m1
                a2 = obtainParam memory (memory ! (ip + 2)) m2
             in if a1 /= 0
                  then machine {getIp = a2}
                  else machine {getIp = ip + 3}
          6 ->
            let a1 = obtainParam memory (memory ! (ip + 1)) m1
                a2 = obtainParam memory (memory ! (ip + 2)) m2
             in if a1 == 0
                  then machine {getIp = a2}
                  else machine {getIp = ip + 3}
          7 ->
            let a1 = obtainParam memory (memory ! (ip + 1)) m1
                a2 = obtainParam memory (memory ! (ip + 2)) m2
                dst = memory ! (ip + 3)
             in machine
                  { getMemory =
                      memory //
                      [ ( dst
                        , if a1 < a2
                            then 1
                            else 0)
                      ]
                  , getIp = ip + 4
                  }
          8 ->
            let a1 = obtainParam memory (memory ! (ip + 1)) m1
                a2 = obtainParam memory (memory ! (ip + 2)) m2
                dst = memory ! (ip + 3)
             in machine
                  { getMemory =
                      memory //
                      [ ( dst
                        , if a1 == a2
                            then 1
                            else 0)
                      ]
                  , getIp = ip + 4
                  }
          99 -> machine {isHalt = True}
          _ -> error $ printf "Unknown opcode `%d` at position `%d`" opcode ip

execute :: Machine -> Machine
execute = head . dropWhile (not . isHalt) . iterate' step
