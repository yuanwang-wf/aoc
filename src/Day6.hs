--
module Day6
  ( partI,
    partII,
  )
where

import Data.List.Split (splitOn)
import qualified Data.Set as Set
import GHC.Base (join)
import System.Directory (getCurrentDirectory)

getInput :: IO [String]
getInput = do
  path <- getCurrentDirectory
  fmap (splitOn "\n\n") $ readFile (path ++ "/data/day6.txt")

getGroupAnswer :: String -> Set.Set Char
getGroupAnswer = foldr Set.insert Set.empty . join . lines

getGroupAnswer' :: String -> Set.Set Char
getGroupAnswer' = intersections . map (foldr Set.insert Set.empty) . lines

intersections :: Ord a => [Set.Set a] -> Set.Set a
intersections [] = Set.empty
intersections [x] = x
intersections (x : y : xs) = Set.intersection x (intersections (y : xs))

partI :: IO Int
partI = fmap (sum . (map (Set.size . getGroupAnswer))) getInput

partII :: IO Int
partII = fmap (sum . (map (Set.size . getGroupAnswer'))) getInput
