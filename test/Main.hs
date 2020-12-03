{-# LANGUAGE TemplateHaskell #-}

module Main where

import Aoc
import Hedgehog
import Hedgehog.Main

prop_test :: Property
prop_test = property $ do
  doAoc === "Advent Of Code 2020"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
