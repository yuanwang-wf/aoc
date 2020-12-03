{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Aoc

prop_test :: Property
prop_test = property $ do
  doAoc === "Aoc"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
