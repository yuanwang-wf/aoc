module Main where

import Criterion.Main
import Day1

-- TODO move this to src, and seperate input, only bench the pure code
main =
  defaultMain
    [ bgroup
        "two sum"
        [ bench "set" $ nfIO solution,
          bench "list com" $ nfIO solution'
        ]
    ]
