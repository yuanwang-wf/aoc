module Day1 where

-- https://adventofcode.com/2020/day/1
-- Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
-- For example, suppose your expense report contained the following:

-- we can implement this using list comp
-- but we can done with a loop
solution1 :: [Int] -> Int
solution1 nums =
  head
    [ (i * j) | i <- nums, j <- nums, i + j == 2020
    ]
