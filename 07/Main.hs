{-# LANGUAGE LambdaCase #-}
module Main where

import Intcode
import Data.Foldable
import Data.List
import Data.Maybe
import Text.Printf
import Debug.Trace

singleCycle :: ([Machine], Maybe Value) -> ([Machine], Maybe Value)
singleCycle (ms, Nothing) = (ms, Nothing)
singleCycle (m0:ms, Just input) =
  let ms' =
        waitForOutput (pushInput input m0) :
        zipWith
          (\(_, input') m ->
             case input' of
               Just input'' -> waitForOutput $ pushInput input'' m
               Nothing -> waitForOutput m)
          ms' ms
   in (map fst ms', snd $ last ms')

primeMachines :: [Setting] -> Machine -> [Machine]
primeMachines settings machine =
  map (\setting -> pushInput setting machine) settings

runAmpLoop :: Machine -> [Value] -> Value
runAmpLoop machine settings =
  last $
  catMaybes $
  takeWhile isJust $
  map snd $ iterate singleCycle (primeMachines settings machine, Just 0)

part2 :: FilePath -> IO Value
part2 filePath = do
  machine <- machineFromFile filePath
  return $ maximum $ map (runAmpLoop machine) $ permutations [5 .. 9]

newtype Amp = Amp
  { runAmp :: Value -> Value
  }

type Setting = Value

newAmp :: Machine -> Setting -> Amp
newAmp machine setting =
  Amp $ \input ->
    head $ getOutput $ execute $ machine {getInput = [setting, input]}

instance Semigroup Amp where
  Amp amp1 <> Amp amp2 = Amp $ amp2 . amp1

instance Monoid Amp where
  mempty = Amp id

runAmpChain :: Machine -> [Value] -> Value
runAmpChain machine settings = runAmp (foldMap (newAmp machine) settings) 0

part1 :: FilePath -> IO Value
part1 filePath = do
  machine <- machineFromFile filePath
  return $ maximum $ map (runAmpChain machine) $ permutations [0 .. 4]

main :: IO ()
main = do
  part1 "./input.txt" >>= printf "Part 1: %d\n"
  part2 "./input.txt" >>= printf "Part 2: %d\n"
