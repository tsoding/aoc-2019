{-# LANGUAGE OverloadedStrings #-}
module Intcode where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf
import Data.List
import Control.Exception
import Control.Monad
import Debug.Trace
import qualified Data.Map.Strict as M
import Data.Maybe

type Address = Integer
type Value = Integer

type Memory = M.Map Address Value

emptyMemory :: Memory
emptyMemory = M.empty

memoryFromImage :: [Value] -> Memory
memoryFromImage xs = M.fromList $ zip [0..] xs

(!) :: Memory -> Address -> Value
(!) memory address = fromMaybe 0 $ M.lookup address memory

(//) :: Memory -> [(Address, Value)] -> Memory
(//) = foldl (\memory (address, value) -> M.insert address value memory)

data Machine = Machine
  { getMemory :: Memory
  , getIp :: Address
  , isHalt :: Bool
  , getInput :: [Integer]
  , getOutput :: [Integer]
  , getBase :: Address
  } deriving (Show)

emptyMachine :: Machine
emptyMachine =
  Machine
    { getMemory = emptyMemory
    , getIp = 0
    , isHalt = False
    , getInput = []
    , getOutput = []
    , getBase = 0
    }

setInput :: [Integer] -> Machine -> Machine
setInput input machine = machine { getInput = input }

setOutput :: [Integer] -> Machine -> Machine
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
machineFromMemory memory = emptyMachine {getMemory = memory}


data Mode
  = Pos
  | Imm
  | Rel
  deriving (Show)
type Opcode = Value
type Param = Value

-- ABCDE
--  1002

decodeMode :: Value -> Mode
decodeMode 0 = Pos
decodeMode 1 = Imm
decodeMode 2 = Rel
decodeMode x = error $ printf "Incorrect parameter mode %d" x

decodeOpcode :: Value -> (Opcode, Mode, Mode, Mode)
decodeOpcode x =
  ( x `mod` 100
  , decodeMode $ x `div` 100 `mod` 10
  , decodeMode $ x `div` 1000 `mod` 10
  , decodeMode $ x `div` 10000 `mod` 10)

obtainParam :: Machine -> Param -> Mode -> Value
obtainParam Machine {getMemory = memory} param Pos = memory ! param
obtainParam _ param Imm = param
obtainParam Machine {getMemory = memory, getBase = base} param Rel =
  memory ! (param + base)

obtainDst :: Machine -> Param -> Mode -> Address
obtainDst _ param Pos = param
obtainDst Machine {getBase = base} param Rel = param + base
obtainDst _ _ Imm =
  error "Immediate mode does not make any sense for the destinations"

t :: String -> a -> a
-- t = trace
t s = id

step :: Machine -> Machine
step machine
  | isHalt machine = machine
  | otherwise =
    let ip = getIp machine
        memory = getMemory machine
        input = getInput machine
        output = getOutput machine
        op = memory ! ip
        base = getBase machine
        (opcode, m1, m2, m3) = decodeOpcode op
     in case opcode of
          1 ->
            let a1 = obtainParam machine (memory ! (ip + 1)) m1
                a2 = obtainParam machine (memory ! (ip + 2)) m2
                dst = obtainDst machine (memory ! (ip + 3)) m3
             in t "plus" $
                machine {getMemory = memory // [(dst, a1 + a2)], getIp = ip + 4}
          2 ->
            let a1 = obtainParam machine (memory ! (ip + 1)) m1
                a2 = obtainParam machine (memory ! (ip + 2)) m2
                dst = obtainDst machine (memory ! (ip + 3)) m3
             in t "mult" $
                machine {getMemory = memory // [(dst, a1 * a2)], getIp = ip + 4}
          3 ->
            case input of
              (x:input') ->
                let dst = obtainDst machine (memory ! (ip + 1)) m1
                 in t "read" $
                    machine
                      { getMemory = memory // [(dst, x)]
                      , getIp = ip + 2
                      , getInput = input'
                      }
              _ ->
                error $ printf "Empty input for read opcode at position %d" ip
          4 ->
            let a1 = obtainParam machine (memory ! (ip + 1)) m1
             in t "write" $ machine {getOutput = a1 : output, getIp = ip + 2}
          5 ->
            let a1 = obtainParam machine (memory ! (ip + 1)) m1
                a2 = obtainParam machine (memory ! (ip + 2)) m2
             in t "if not-equal" $
                if a1 /= 0
                  then machine {getIp = a2}
                  else machine {getIp = ip + 3}
          6 ->
            let a1 = obtainParam machine (memory ! (ip + 1)) m1
                a2 = obtainParam machine (memory ! (ip + 2)) m2
             in t "if equal-zero" $
                if a1 == 0
                  then machine {getIp = a2}
                  else machine {getIp = ip + 3}
          7 ->
            let a1 = obtainParam machine (memory ! (ip + 1)) m1
                a2 = obtainParam machine (memory ! (ip + 2)) m2
                dst = obtainDst machine (memory ! (ip + 3)) m3
             in t "less than" $
                machine
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
            let a1 = obtainParam machine (memory ! (ip + 1)) m1
                a2 = obtainParam machine (memory ! (ip + 2)) m2
                dst = obtainDst machine (memory ! (ip + 3)) m3
             in t "equal" $
                machine
                  { getMemory =
                      memory //
                      [ ( dst
                        , if a1 == a2
                            then 1
                            else 0)
                      ]
                  , getIp = ip + 4
                  }
          9 ->
            let a1 = obtainParam machine (memory ! (ip + 1)) m1
             in t (printf "adjust relative base: %d" a1) $
                machine {getBase = base + a1, getIp = ip + 2}
          99 -> t "halt" $ machine {isHalt = True}
          _ -> error $ printf "Unknown opcode `%d` at position `%d`" opcode ip

execute :: Machine -> Machine
execute = head . dropWhile (not . isHalt) . iterate step

popOutput :: Machine -> (Machine, Maybe Value)
popOutput machine@Machine {getOutput = output:restOutput} =
  (machine {getOutput = restOutput}, Just output)
popOutput machine = (machine, Nothing)

pushInput :: Value -> Machine -> Machine
pushInput input machine@Machine {getInput = restInput} =
  machine {getInput = restInput ++ [input]}

isAboutToRead :: Machine -> Bool
isAboutToRead machine =
  let ip = getIp machine
      memory = getMemory machine
      op = memory ! ip
      (opcode, _, _, _) = decodeOpcode op
   in opcode == 3

waitForInput :: Machine -> Machine
waitForInput machine =
  head $ dropWhile (not . isTimeToStop) $ iterate step machine
  where
    isTimeToStop m = isHalt m || (isAboutToRead m && null (getInput m))

waitForOutput :: Machine -> (Machine, Maybe Value)
waitForOutput machine =
  popOutput $
  head $
  dropWhile
    (\m -> not (isHalt m) && null (getOutput m)) $
  iterate step machine

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
