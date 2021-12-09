module Parsec101
  ( simple
  , run
  , parens
  , openClose
  , testOr
  , testOr1
  , testOr2
  )
  where

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

run :: Show a => P.Parser a -> String -> IO ()
run p input
  = case P.parse p "" input of
      Left err -> do{ putStr "parse error at "
                    ; print err
                    }
      Right x -> print x

-- This function will identify
-- if the sent character is a letter or not
simple :: P.Parser Char
simple = P.letter

-- openClose identify 
openClose :: P.Parser Char
openClose = do{ P.char '('
              ; P.char ')'
              }

parens :: P.Parser ()
parens = do{ P.char '('
           ; parens
           ; P.char ')'
           ; parens
           }
         P.<|> return ()


testOr :: P.Parser [Char]
testOr = P.string "(a)"
         P.<|> P.string "(b)"

testOr1 :: P.Parser Char
testOr1 = do{ P.char '(' 
            ; P.char 'a' P.<|> P.char 'b'
            ; P.char ')'
            }

testOr2 :: P.Parser [Char]
testOr2 = P.try (P.string "(a)")
          P.<|> P.string "(b)"

