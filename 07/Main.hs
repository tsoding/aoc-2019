module Main where

import Intcode
import Data.Foldable
import Data.List

newtype Amp = Amp
  { runAmp :: Int -> Int
  }

type Setting = Int

newAmp :: Machine -> Setting -> Amp
newAmp machine setting =
  Amp $ \input ->
    head $ getOutput $ execute $ machine {getInput = [setting, input]}

instance Semigroup Amp where
  Amp amp1 <> Amp amp2 = Amp $ amp2 . amp1

instance Monoid Amp where
  mempty = Amp id

runAmpChain :: Machine -> [Int] -> Int
runAmpChain machine settings = runAmp (foldMap (newAmp machine) settings) 0

part1 :: FilePath -> IO Int
part1 filePath = do
  machine <- machineFromFile filePath
  return $ maximum $ map (runAmpChain machine) $ permutations [0 .. 4]

main :: IO ()
main = undefined
