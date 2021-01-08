{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Function
import Data.Maybe
import Text.Printf

type Name = T.Text

data OrbitsTree = OrbitsTree
  { otMap :: M.Map Name [Name]
  } deriving Show

orbitsTreeFromFile :: FilePath -> IO OrbitsTree
orbitsTreeFromFile filePath = do
  content <- T.readFile filePath
  content &
    T.lines &
    map (T.splitOn ")") &
    map (\[a, b] -> (a, b)) &
    sortBy (compare `on` fst) &
    groupBy ((==) `on` fst) &
    map (\ps -> (fst $ head ps, map snd ps)) &
    M.fromList &
    OrbitsTree &
    return

solveDfs :: OrbitsTree -> Name -> Int
solveDfs orbits = solveDfs' 0
  where
    solveDfs' acc parent =
      case M.lookup parent $ otMap orbits of
        Nothing -> acc
        Just children ->
          acc + (foldl' (+) 0 $ map (solveDfs' (acc + 1)) children)

data OrbitsMap = OrbitsMap
  { omMap :: M.Map Name [Name]
  } deriving Show

orbitsMapFromFile :: FilePath -> IO OrbitsMap
orbitsMapFromFile filePath = do
  content <- T.readFile filePath
  content &
    T.lines &
    map (T.splitOn ")") &
    concatMap (\[a, b] -> [(a, b), (b, a)]) &
    sortBy (compare `on` fst) &
    groupBy ((==) `on` fst) &
    map (\ps -> (fst $ head ps, map snd ps)) &
    M.fromList &
    OrbitsMap &
    return

solveBfs :: OrbitsMap -> Int
solveBfs orbits = solveBfs' [(start, 0)] S.empty
  where
    solveBfs' :: [(Name, Int)] -> S.Set Name -> Int
    solveBfs' wave visited =
      case filter ((== end) . fst) wave of
        [(_, answer)] -> answer
        [] ->
          let notVisited = filter (\(name, _) -> S.notMember name visited) wave
           in solveBfs'
                (concatMap
                   (\(name, path) ->
                      map (\child -> (child, path + 1)) $
                      concat $ maybeToList $ M.lookup name (omMap orbits))
                   notVisited)
                (S.union visited $ S.fromList $ map fst notVisited)
    Just [start] = M.lookup "YOU" $ omMap orbits
    Just [end] = M.lookup "SAN" $ omMap orbits

main :: IO ()
main = do
  solveDfs <$> orbitsTreeFromFile "input.txt" <*> return "COM" >>= printf "Part 1: %d\n"
  solveBfs <$> orbitsMapFromFile "input.txt" >>= printf "Part 2: %d\n"
