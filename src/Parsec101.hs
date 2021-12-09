module Parsec101
  ( simple
  , run
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

