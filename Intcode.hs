{-# LANGUAGE OverloadedStrings #-}
module Intcode where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Array
import Text.Printf
import Data.List
import Control.Exception
import Control.Monad

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

test :: IO ()
test = do
  let equalTo8Pos =
        ( [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
        , "Using position mode, consider whether the input is equal to 8; \
          \output 1 (if it is) or 0 (if it is not)." :: String
        , [([8], [1]), ([7], [0]), ([10], [0])])
  let lessThan8Pos =
        ( [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]
        , "Using position mode, consider whether the input is less than 8; \
          \output 1 (if it is) or 0 (if it is not)."
        , [([8], [0]), ([9], [0]), ([7], [1])])
  let equalTo8Imm =
        ( [3, 3, 1108, -1, 8, 3, 4, 3, 99]
        , "Using position mode, consider whether the input is equal to 8; \
          \output 1 (if it is) or 0 (if it is not)." :: String
        , [([8], [1]), ([7], [0]), ([10], [0])])
  let lessThan8Imm =
        ( [3, 3, 1107, -1, 8, 3, 4, 3, 99]
        , "Using position mode, consider whether the input is less than 8; \
          \output 1 (if it is) or 0 (if it is not)."
        , [([8], [0]), ([9], [0]), ([7], [1])])
  let jumpPos =
        ( [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]
        , "Take an input, then output 0 if the input was zero or 1 \
          \if the input was non-zero (position mode)"
        , [([0], [0]), ([1], [1]), ([2], [1])])
  let jumpImm =
        ( [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]
        , "Take an input, then output 0 if the input was zero or 1 \
          \if the input was non-zero (immediate mode)"
        , [([0], [0]), ([1], [1]), ([2], [1])])
  let cmpAndJump =
        ( [ 3
          , 21
          , 1008
          , 21
          , 8
          , 20
          , 1005
          , 20
          , 22
          , 107
          , 8
          , 21
          , 20
          , 1006
          , 20
          , 31
          , 1106
          , 0
          , 36
          , 98
          , 0
          , 0
          , 1002
          , 21
          , 125
          , 20
          , 4
          , 20
          , 1105
          , 1
          , 46
          , 104
          , 999
          , 1105
          , 1
          , 46
          , 1101
          , 1000
          , 1
          , 20
          , 4
          , 20
          , 1105
          , 1
          , 46
          , 98
          , 99
          ]
        , "Testing comparison and jumps at the same time"
        , [([7], [999]), ([8], [1000]), ([9], [1001])])
  let testGroups =
        [ equalTo8Pos
        , lessThan8Pos
        , equalTo8Imm
        , lessThan8Imm
        , jumpPos
        , jumpImm
        , cmpAndJump
        ]
  forM_ testGroups $ \(program, description, testCases) -> do
    printf "Description: %s\n" description
    printf "Program: %s\n" $ show program
    forM_ testCases $ \(input, expected) -> do
      printf "  Input: %s\n" $ show input
      printf "  Expected: %s\n" $ show expected
      let actual =
            getOutput $
            execute $
            setInput input $ machineFromMemory $ memoryFromImage program
      printf "  Actual: %s\n" $ show actual
      assert (expected == actual) $ putStrLn "    OK"
