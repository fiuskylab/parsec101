module Main where

import Lib
import Parsec101

main :: IO ()
main = do
  putStrLn "-----| simple |-----"
  run simple "a"
  run simple "-"
  putStrLn "-----| parens |-----"
  run parens "(())()"
  run parens "(()()"
  putStrLn "-----| testOr |-----"
  run testOr "(a)"
  run testOr "(b)"
