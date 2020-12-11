-- |
module Day4 where

import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import System.Directory (getCurrentDirectory)

data Byr = Byr String deriving (Show)

data Iyr = Iyr String deriving (Show)

data Eyr = Eyr String deriving (Show)

data Hgt = Hgt String deriving (Show)

data Hcl = Hcl String deriving (Show)

data Ecl = Ecl String deriving (Show)

data Pid = Pid String deriving (Show)

data Cid = Cid String deriving (Show)

data Passport = Passport {byr :: Maybe Byr, iyr :: Maybe Iyr, eyr :: Maybe Eyr, hgt :: Maybe Hgt, hcl :: Maybe Hcl, ecl :: Maybe Ecl, pid :: Maybe Pid, cid :: Maybe Cid} deriving (Show)

parsePart :: String -> Passport -> Passport
parsePart ctn p = case splitOn ":" ctn of
  ["byr", x] -> p {byr = Just (Byr x)}
  ["iyr", x] -> p {iyr = Just (Iyr x)}
  ["eyr", x] -> p {eyr = Just (Eyr x)}
  ["hgt", x] -> p {hgt = Just (Hgt x)}
  ["hcl", x] -> p {hcl = Just (Hcl x)}
  ["ecl", x] -> p {ecl = Just (Ecl x)}
  ["pid", x] -> p {pid = Just (Pid x)}
  ["cid", x] -> p {cid = Just (Cid x)}
  _ -> p

zero :: Passport
zero = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

parsePassport :: String -> Passport
parsePassport cnt = foldr parsePart zero $ (splitOn "\n" cnt) >>= splitOn " "

parsePassports :: String -> [Passport]
parsePassports = map parsePassport . splitOn "\n\n"

isValid :: Passport -> Bool
isValid p = foldr (\f v -> f p && v) True ([isJust . byr, isJust . iyr, isJust . eyr, isJust . hgt, isJust . hcl, isJust . ecl, isJust . pid] :: [Passport -> Bool])

getPassport :: IO [Passport]
getPassport = do
  path <- getCurrentDirectory
  content <- readFile (path ++ "/data/day4.txt")
  return (parsePassports content)

test :: IO ()
test = do
  passports <- getPassport
  print $ foldr (\p sum -> if isValid p then sum + 1 else sum) (0 :: Int) passports
