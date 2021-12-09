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
  putStrLn "-----| testOr1 |-----"
  run testOr1 "(a)"
  run testOr1 "(b)"
  putStrLn "-----| testOr2 |-----"
  run testOr2 "(a)"
  run testOr2 "(b)"
  putStrLn "-----| testOr3 |-----"
  run testOr3 "(a)"
  run testOr3 "(b)"
  putStrLn "-----| nesting |-----"
  run nesting "(())()"
  run nesting "(()(()))"
  putStrLn "-----| nesting1 |-----"
  run nesting1 "(())()"
  run nesting1 "(()(()))"
  putStrLn "-----| word |-----"
  run word "foo"
  run word "foo bar baz"
  putStrLn "-----| word1 |-----"
  run word1 "foo"
  run word1 "foo bar baz"
