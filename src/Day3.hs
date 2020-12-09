-- |
module Day3 (test, test') where

import System.Directory (getCurrentDirectory)

type Grid = [[Bool]]

type Pos = (Int, Int)

type Slope = (Int, Int)

parseGrid :: String -> Grid
parseGrid = map (map (== '#')) . lines

reachTheEnd :: Pos -> Grid -> Bool
reachTheEnd (_, y) grid = (y + 1) >= (length grid)

move :: Pos -> Slope -> Grid -> (Pos, Bool)
move (x, y) (r, d) grid = ((x', y'), p)
  where
    wide = (length . head) grid
    y' = y + d
    x' = x + r
    p = (grid !! y') !! (x' `mod` wide)

counterTree :: Grid -> Slope -> Int
counterTree grid s = helper grid (0, 0) s 0

helper :: Grid -> Pos -> Slope -> Int -> Int
helper grid pos s acum = if (reachTheEnd pos grid) then acum else helper grid pos' s (if tree then acum + 1 else acum)
  where
    (pos', tree) = move pos s grid

test :: IO ()
test = do
  path <- getCurrentDirectory
  content <- readFile (path ++ "/data/day3.txt")
  print (counterTree (parseGrid content) (3, 1))

test' :: IO ()
test' = do
  path <- getCurrentDirectory
  content <- readFile (path ++ "/data/day3.txt")
  print $ foldr (*) 1 (map (\s -> counterTree (parseGrid content) s) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)])
