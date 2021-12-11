{-# LANGUAGE FlexibleContexts #-}

module Parsec102
  ( expr
  )
  where

import Text.Parsec.Expr
import Text.Parsec
import Text.Parsec.String
import Data.Functor.Identity (Identity)

-- expr   ::= expr '+' term | term
-- term   ::= term '*' factor | factor
-- factor ::= '(' expr ')' | digit+
-- digit  ::= '0' | '1' | ... | '9'

expr :: Parser Integer
expr = buildExpressionParser table factor
     <?> "expression"

table :: OperatorTable String () Identity Integer
table = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
        ]
      where
        op s f assoc
          = Infix (do{ string s; return f}) assoc

factor :: ParsecT String () Identity Integer
factor = do{ char '('
           ; x <- expr
           ; char ')'
           ; return x}
           <|> number
           <?> "simple expression"

number :: Parser Integer
number = do{ ds <- many1 digit
            ; return (read ds)
            }
            <?> "number"
