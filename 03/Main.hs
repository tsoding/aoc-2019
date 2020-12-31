module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Array
import Data.List
import Text.Printf
import Data.Maybe
import Data.Function

type Cell = (Int, Int)

data Seg
  = Hor Int
        (Int, Int)
  | Ver Int
        (Int, Int)
  deriving (Show)

inter :: Seg -> Seg -> Maybe Cell
inter (Hor y (x1, x2)) (Ver x (y1, y2))
  | x1 <= x && x <= x2 && y1 <= y && y <= y2 = Just (x, y)
  | otherwise = Nothing
inter ver@(Ver _ _) hor@(Hor _ _) = inter hor ver
inter _ _ = Nothing

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

parseDir :: T.Text -> Dir
parseDir input =
  case T.uncons input of
    Just ('R', s) -> R $ read $ T.unpack s
    Just ('L', s) -> L $ read $ T.unpack s
    Just ('U', s) -> U $ read $ T.unpack s
    Just ('D', s) -> D $ read $ T.unpack s
    _ -> error $ printf "`%s` is not a correct direction" input

parseWire :: T.Text -> [Seg]
parseWire = segsOfWire . map parseDir . T.splitOn ","

readInput :: FilePath -> IO ([Seg], [Seg])
readInput filePath = do
  first:second:_ <- T.lines <$> T.readFile filePath
  return $ (parseWire first, parseWire second)

dist :: (Int, Int) -> Int
dist (x, y) = abs x + abs y

inters :: ([Seg], [Seg]) -> [(Int, Int)]
inters (xs, ys) = catMaybes [inter x y | x <- xs, y <- ys]

part1 :: ([Seg], [Seg]) -> Int
part1 (xs, ys) = head $ filter (> 0) $ sort $ map dist $ inters (xs, ys)

main :: IO ()
main = undefined
