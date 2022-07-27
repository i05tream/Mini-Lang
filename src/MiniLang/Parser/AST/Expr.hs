{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module MiniLang.Parser.AST.Expr where

import MiniLang.Data.AST.Expr (
    Expr (Add, Sub, Mul, Div)
  , Value (Value)
  )

import MiniLang.Parser (Parser, number, char)

value :: Parser Value
value = Value <$> number

add :: Parser Expr
add = do
  v <- value
  char '+'
  Add v <$> value

sub :: Parser Expr
sub = do
  v <- value
  char '-'
  Sub v <$> value
