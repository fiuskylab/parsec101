module Main where

import Lib
import Parsec101

main :: IO ()
main = do
  run simple "a"
  run simple "-"
