module Parsec101
  ( simple
  , run
  , parens
  , openClose
  , testOr
  , testOr1
  , testOr2
  , testOr3
  , nesting
  , nesting1
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


testOr3 :: P.Parser [Char]
testOr3 = do{ P.try (P.string "(a")
                    ; P.char ')'
                    ; return "(a)"
            } P.<|> P.string "(b)"


nesting :: P.Parser Int
nesting = do{ P.char '(' 
            ; n <- nesting
            ; P.char ')'
            ; m <- nesting
            ; return (max (n+1) m)
            }
          P.<|> return 0

nesting1 :: P.Parser Int
nesting1 = do{ P.char '(' 
            ; n <- nesting
            ; P.char ')'
            ; max (n+1) <$> nesting
            }

-- Now let's start trying to create
-- a more pratical example
-- :ricardinst!ricardinst@ricardinst.tmi.twitch.tv PRIVMSG #rafiusky :Shizukani shite kudasai!
username :: P.Parser Char
username = do{ P.char ':'
             ; P.letter
             ; P.char '!'
             }

-- PING :tmi.twitch.tv
-- :tmi.twitch.tv 004 rafiuskybot :-
