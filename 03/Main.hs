{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Array
import Data.List
import Text.Printf
import Data.Maybe
import Data.Function

type Steps = Int
type Cell = (Int, Int)

(^-^) :: Cell -> Cell -> Cell
(^-^) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

data Seg
  = Hor Int
        (Int, Int)
  | Ver Int
        (Int, Int)
  deriving (Show)

type Vec = (Cell, Steps, Dir)

vec2Seg :: Vec -> Seg
vec2Seg (begin, _, dir) = mkSeg begin $ moveTo begin dir

interVecs :: Vec -> Vec -> Maybe (Cell, Steps)
interVecs vec1@(begin1, steps1, _) vec2@(begin2, steps2, _) = do
  end <- interSegs (vec2Seg vec1) (vec2Seg vec2)
  let steps = steps1 + steps2 + dist (end ^-^ begin1) + dist (end ^-^ begin2)
  return (end, steps)

interSegs :: Seg -> Seg -> Maybe Cell
interSegs (Hor y (x1, x2)) (Ver x (y1, y2))
  | x1 <= x && x <= x2 && y1 <= y && y <= y2 = Just (x, y)
  | otherwise = Nothing
interSegs ver@(Ver _ _) hor@(Hor _ _) = interSegs hor ver
interSegs _ _ = Nothing

data Dir
  = R Int
  | L Int
  | U Int
  | D Int
  deriving (Show)

mkSeg :: Cell -> Cell -> Seg
mkSeg (x1, y1) (x2, y2)
  | x1 == x2 = Ver x1 $ fix (y1, y2)
  | y1 == y2 = Hor y1 $ fix (x1, x2)
  | otherwise = error "Invalid seg"
  where
    fix :: (Int, Int) -> (Int, Int)
    fix (x1, x2)
      | x1 > x2 = (x2, x1)
      | otherwise = (x1, x2)

moveTo :: Cell -> Dir -> Cell
moveTo (x, y) (R n) = (x + n, y)
moveTo (x, y) (L n) = (x - n, y)
moveTo (x, y) (U n) = (x, y + n)
moveTo (x, y) (D n) = (x, y - n)

segsOfWire :: [Dir] -> [Seg]
segsOfWire dirs = zipWith mkSeg ps (tail ps)
  where
    ps = scanl moveTo (0, 0) dirs

vecsOfWire :: [Dir] -> [Vec]
vecsOfWire dirs = zip3 ps ss dirs
  where
    ps = scanl moveTo (0, 0) dirs
    ss =
      scanl (\steps (begin, end) -> dist (end ^-^ begin) + steps) 0 $
      zip ps (tail ps)

parseDir :: T.Text -> Dir
parseDir input =
  case T.uncons input of
    Just ('R', s) -> R $ read $ T.unpack s
    Just ('L', s) -> L $ read $ T.unpack s
    Just ('U', s) -> U $ read $ T.unpack s
    Just ('D', s) -> D $ read $ T.unpack s
    _ -> error $ printf "`%s` is not a correct direction" input

parseWire :: T.Text -> [Dir]
parseWire = map parseDir . T.splitOn ","

readInput :: FilePath -> IO ([Dir], [Dir])
readInput filePath = do
  first:second:_ <- T.lines <$> T.readFile filePath
  return $ (parseWire first, parseWire second)

dist :: (Int, Int) -> Int
dist (x, y) = abs x + abs y

allSegsInters :: ([Seg], [Seg]) -> [(Int, Int)]
allSegsInters (xs, ys) = catMaybes [interSegs x y | x <- xs, y <- ys]

part1 :: ([Dir], [Dir]) -> Int
part1 (xs, ys) =
  head $
  filter (> 0) $ sort $ map dist $ allSegsInters (segsOfWire xs, segsOfWire ys)

allVecsInters :: ([Vec], [Vec]) -> [Steps]
allVecsInters (xs, ys) = map snd $ catMaybes [interVecs x y | x <- xs, y <- ys]

part2 :: ([Dir], [Dir]) -> Steps
part2 (xs, ys) =
  head $ filter (> 0) $ sort $ allVecsInters (vecsOfWire xs, vecsOfWire ys)

main :: IO ()
main = do
  input <- readInput "input.txt"
  printf "Part 1: %d\n" $ part1 input
  printf "Part 2: %d\n" $ part2 input
