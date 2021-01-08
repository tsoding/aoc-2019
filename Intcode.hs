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
  } deriving (Show)

memoryFromFile :: FilePath -> IO Memory
memoryFromFile filePath = do
  xs <-
    map (read . T.unpack . T.strip) . T.splitOn "," <$> T.readFile filePath
  let n = length xs
  return $ array (0, n - 1) (zip [0 ..] xs)

machineFromFile :: FilePath -> IO Machine
machineFromFile filePath = do
  memory <- memoryFromFile filePath
  return $ Machine memory 0 False

step :: Machine -> Machine
step machine@(Machine _ _ True) = machine
step machine@(Machine memory ip _) =
  let opcode = memory ! getIp machine
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
        99 -> machine {isHalt = True}
        _ -> error $ printf "Unknown opcode `%d` at position `%d`" opcode ip

execute :: Machine -> Machine
execute = head . dropWhile (not . isHalt) . iterate' step
