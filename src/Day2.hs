{-# LANGUAGE OverloadedStrings #-}

-- | https://adventofcode.com/2020/day/2
module Day2 where

import Control.Applicative (Alternative (many))
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory)

-- We need a parser to parse password policy
-- 1-3 a: abcde
-- num dash num: and a string
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec

data PWRecord = PWRecord {c :: Char, minL :: Int, maxL :: Int, pw :: T.Text} deriving (Show)

validPassword :: PWRecord -> Bool
validPassword = undefined

parseRecord :: Parser PWRecord
parseRecord =
  do
    minLength <- decimal
    char '-'
    maxLength <- decimal
    char ' '
    c <- anyChar
    string ": "
    pw <- takeText
    return $ PWRecord c minLength maxLength pw

recordParser :: Parser [PWRecord]
recordParser = many $ parseRecord <* endOfInput

test :: IO ()
test = print $ parseOnly parseRecord "15-19 k: kkkkkkkkkkkkzkkkkkkk"

test'' :: IO ()
test'' = print $ parseOnly recordParser "15-19 k: kkkkkkkkkkkkzkkkkkkk\n15-19 k: kkkkkkkkkkkkzkkkkkkk\n"

test' :: IO ()
test' = do
  path <- getCurrentDirectory
  content <- TIO.readFile (path ++ "/data/day2.txt")
  print $ map (parseOnly parseRecord) (T.lines content)
