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

-- TODO: parameter modes
step :: Machine -> Machine
step machine
  | isHalt machine = machine
  | otherwise =
    let ip = getIp machine
        memory = getMemory machine
        input = getInput machine
        output = getOutput machine
        opcode = memory ! ip
     in case opcode of
          1 ->
            let a1 = memory ! (memory ! (ip + 1))
                a2 = memory ! (memory ! (ip + 2))
                dst = memory ! (ip + 3)
             in machine {getMemory = memory // [(dst, a1 + a2)], getIp = ip + 4}
          2 ->
            let a1 = memory ! (memory ! (ip + 1))
                a2 = memory ! (memory ! (ip + 2))
                dst = memory ! (ip + 3)
             in machine {getMemory = memory // [(dst, a1 * a2)], getIp = ip + 4}
          3 ->
            case input of
              (x:input') ->
                let p = memory ! (ip + 1)
                 in machine
                      { getMemory = memory // [(p, x)]
                      , getIp = ip + 2
                      , getInput = input'
                      }
              _ ->
                error $ printf "Empty input for read opcode at position %d" ip
          4 ->
            let x = memory ! (memory ! (ip + 1))
             in machine {getOutput = x : output, getIp = ip + 2}
          99 -> machine {isHalt = True}
          _ -> error $ printf "Unknown opcode `%d` at position `%d`" opcode ip

execute :: Machine -> Machine
execute = head . dropWhile (not . isHalt) . iterate' step
