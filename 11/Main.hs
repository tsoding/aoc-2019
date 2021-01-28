module Main where

import Intcode
import qualified Data.Set as S
import Text.Printf

data V2 =
  V2 Int
     Int
  deriving (Show, Eq, Ord)

data Dir
  = U
  | R
  | D
  | L
  deriving (Show, Enum)

data Turn
  = TL
  | TR
  deriving (Show, Enum)

data Color
  = Black
  | White
  deriving (Show, Enum)

turnTo :: Turn -> Dir -> Dir
turnTo delta dir =
  case delta of
    TL -> toEnum ((fromEnum dir - 1) `mod` n)
    TR -> toEnum ((fromEnum dir + 1) `mod` n)
  where n = length $ enumFrom (toEnum 0 :: Dir)

stepTo :: Dir -> V2 -> V2
stepTo U (V2 x y) = V2 x (y - 1)
stepTo L (V2 x y) = V2 (x - 1) y
stepTo R (V2 x y) = V2 (x + 1) y
stepTo D (V2 x y) = V2 x (y + 1)

data State = State
  { robotPos :: V2
  , robotDir :: Dir
  , robotBrain :: Machine
  , whitePanels :: S.Set V2
  , blackPanels :: S.Set V2
  , touchedPanels :: S.Set V2
  } deriving (Show)

mkState :: Machine -> State
mkState machine =
  State
    { robotPos = V2 0 0
    , robotDir = toEnum 0
    , robotBrain = waitForInput machine
    , whitePanels = S.empty
    , blackPanels = S.empty
    , touchedPanels = S.empty
    }

isBrainHalt :: State -> Bool
isBrainHalt = isHalt . robotBrain

nextState :: Color -> State -> State

nextState defaultColor state@State { robotPos = pos
                                   , robotDir = dir
                                   , robotBrain = brain
                                   , whitePanels = ws
                                   , blackPanels = bs
                                   , touchedPanels = ts
                                   }
  | isHalt brain = state
  | otherwise =
    let panel = case defaultColor of
                  Black -> if S.member pos ws then 1 else 0
                  White -> if S.member pos bs then 0 else 1
        (brain', Just color) = waitForOutput $ pushInput panel brain
        (brain'', Just turn) = waitForOutput $ brain'
        dir' = turnTo (toEnum $ fromIntegral turn) dir
     in state
          { robotDir = dir'
          , robotPos = stepTo dir' pos
          , robotBrain = waitForInput brain''
          , whitePanels =
              case toEnum $ fromIntegral color of
                Black -> S.delete pos ws
                White -> S.insert pos ws
          , blackPanels =
              case toEnum $ fromIntegral color of
                Black -> S.insert pos bs
                White -> S.delete pos bs
          , touchedPanels = S.insert pos ts
          }

part2 :: Machine -> String
part2 machine =
  let state =
        head $
        dropWhile (not . isBrainHalt) $
        iterate (nextState White) $ mkState machine
      bs = blackPanels state
   in unlines
        [ [ (if S.member (V2 x y) bs
               then '.'
               else '#')
        | x <- [0 .. 50]
        ]
        | y <- [0 .. 7]
        ]

part1 :: Machine -> Int
part1 machine =
  S.size $
  touchedPanels $
  head $
  dropWhile (not . isBrainHalt) $ iterate (nextState Black) $ mkState machine

main :: IO ()
main = do
  machine <- machineFromFile "./input.txt"
  printf "Part 1: %d\n" $ part1 machine
  printf "Part 2:\n"
  putStrLn $ part2 machine
