module Main where

import Criterion.Main
import Day1

main =
  defaultMain
    [ bgroup
        "two sum"
        [ bench "set" $ nfIO solution,
          bench "list com" $ nfIO solution'
        ]
    ]
