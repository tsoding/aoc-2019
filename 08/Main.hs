module Main where

import Data.List
import Data.Function
import Text.Printf

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = take n xs : chunks n (drop n xs)

countElems :: Eq a => a -> [a] -> Int
countElems x = length . filter (==x)

mix :: Char -> Char -> Char
mix a '2' = a
mix _ b = b

mixLayer :: String -> String -> String
mixLayer = zipWith mix

renderLayers :: [String] -> String
renderLayers = foldl1 mixLayer

printImage :: Int -> String -> IO ()
printImage width image = mapM_ putStrLn $ chunks width image

part1 :: (Int, Int) -> String -> Int
part1 (width, height) input = countElems '1' m * countElems '2' m
  where
    n = width * height
    m = minimumBy (compare `on` (countElems '0')) $ chunks n input

part2 :: (Int, Int) -> String -> IO ()
part2 (width, height) input =
  printImage 25 $ renderLayers $ reverse $ chunks (25 * 6) input

main :: IO ()
main = do
  input <- head . lines <$> readFile "./input.txt"
  printf "Part 1: %d\n" $ part1 (25, 6) input
  printf "Part 2: \n"
  part2 (25, 6) input
