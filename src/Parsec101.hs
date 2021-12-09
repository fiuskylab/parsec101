module Parsec101
  ( simple
  , run
  , parens
  , openClose
  , testOr
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

