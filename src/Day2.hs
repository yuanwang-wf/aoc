-- | https://adventofcode.com/2020/day/2
module Day2 where

-- We need a parser to parse password policy
-- 1-3 a: abcde
-- num dash num: and a string
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec

data PWRecord = PWRecord {c :: Char, minL :: Int, maxL :: Int, pw :: String}
