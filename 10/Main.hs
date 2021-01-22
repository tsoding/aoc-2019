module Main where

import Data.List
import Data.Function
import Text.Printf
import Data.Fixed

data V2 =
  V2 Int
     Int
  deriving (Eq, Show)

type Region = [V2]

(^-^) :: V2 -> V2 -> V2
(^-^) (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

type Rad = Double

parseRegion :: String -> Region
parseRegion =
  concatMap
    (\(y, xs) -> map (\x -> V2 x y) $ map fst $ filter ((== '#') . snd) xs) .
  zip [0 ..] . map (zip [0 ..]) . lines

angle :: V2 -> Rad
angle (V2 x y) = (result + half_pi) `mod'` double_pi
  where
     result = atan2 (fromIntegral y) (fromIntegral x)
     half_pi = pi / 2.0
     double_pi = pi * 2.0

len :: V2 -> Double
len (V2 x y) = sqrt $ fromIntegral (x^2 + y^2)

dist :: V2 -> V2 -> Double
dist a b = len $ b ^-^ a

angleBetween :: V2 -> V2 -> Rad
angleBetween a b = angle $ b ^-^ a

countVisibleFrom :: V2 -> Region -> Int
countVisibleFrom a region = length $ group $ sort $ map (angleBetween a) region

part1 :: Region -> (V2, Int)
part1 region =
  maximumBy
    (compare `on` snd)
    [(a, countVisibleFrom a $ filter (/= a) region) | a <- region]

part2 :: V2 -> Region -> Int
part2 a region = x * 100 + y
  where
    V2 x y = head (ys !! 199)
    ys = xs ++ (map tail $ filter (not . null) ys)
    xs =
      map (sortBy (compare `on` dist a)) $
      groupBy ((==) `on` angleBetween a) $
      sortBy (compare `on` angleBetween a) $ filter (/= a) region

main :: IO ()
main = do
  region <- parseRegion <$> readFile "./input.txt"
  let (a, answer1) = part1 region
  printf "Part 1: %d\n" answer1
  printf "Part 2: %d\n" $ part2 a region
