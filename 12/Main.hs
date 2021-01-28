{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf

data V3 =
  V3 Int
     Int
     Int
  deriving (Show, Eq, Ord)

(^+^) :: V3 -> V3 -> V3
(^+^) (V3 x y z) (V3 a b c) = V3 (x + a) (y + b) (z + c)

foldV3 :: (Int -> Int -> Int) -> V3 -> Int
foldV3 f (V3 x y z) = f (f x y) z

mapV3 :: (Int -> Int) -> V3 -> V3
mapV3 f (V3 x y z) = V3 (f x) (f y) (f z)

data Moon = Moon
  { moonPos :: V3
  , moonVel :: V3
  } deriving (Show)

data System = System
  { moons :: [Moon]
  } deriving (Show)

mkMoon :: V3 -> Moon
mkMoon pos = Moon {moonPos = pos, moonVel = V3 0 0 0}

parseV3 :: T.Text -> V3
parseV3 input = V3 x y z
  where
    [x, y, z] =
      map (read . T.unpack . (!! 1) . T.splitOn "=") $
      T.splitOn ", " $ T.dropAround (`elem` ['<', '>']) input

systemFromFile :: FilePath -> IO System
systemFromFile filePath =
  System . map (mkMoon . parseV3) . T.lines <$> T.readFile filePath

simulate :: System -> System
simulate system = undefined

gravity :: Int -> Int -> Int -> Int
gravity v x1 x2
  | x1 < x2 = v + 1
  | x1 > x2 = v - 1
  | otherwise = v

gravityV3 :: V3 -> V3 -> V3 -> V3
gravityV3 (V3 vx vy vz) (V3 x1 y1 z1) (V3 x2 y2 z2) =
  V3 (gravity vx x1 x2) (gravity vy y1 y2) (gravity vz z1 z2)

gravityMoon :: Moon -> Moon -> Moon
gravityMoon moon1 moon2 =
  moon1 {moonVel = gravityV3 (moonVel moon1) (moonPos moon1) (moonPos moon2)}

gravitySystem :: System -> System
gravitySystem system =
  system
    { moons =
        map
          (\(i, moon) ->
             foldl gravityMoon moon $ map snd $ removeIndex i indexedMoons)
          indexedMoons
    }
  where
    indexedMoons = zip [0 ..] $ moons system
    removeIndex i = filter ((/= i) . fst)

velocityMoon :: Moon -> Moon
velocityMoon moon@Moon {moonPos = pos, moonVel = vel} =
  moon {moonPos = pos ^+^ vel}

velocitySystem :: System -> System
velocitySystem system = system {moons = map velocityMoon $ moons system}

stepSystem :: System -> System
stepSystem = velocitySystem . gravitySystem

energyV3 :: V3 -> Int
energyV3 = foldV3 (+) . mapV3 abs

energyMoon :: Moon -> Int
energyMoon Moon {moonPos = pos, moonVel = vel} = energyV3 pos * energyV3 vel

energySystem :: System -> Int
energySystem system = sum $ map energyMoon $ moons system

part1 :: Int -> System -> Int
part1 n system = energySystem $ (iterate stepSystem system) !! n

main :: IO ()
main = do
  system <- systemFromFile "./input.txt"
  printf "Part 1: %d\n" $ part1 1000 system
