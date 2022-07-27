{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module MiniLang.Parser.AST.Expr where

import Control.Applicative ((<|>))
import MiniLang.Data.AST.Expr (
    Expr (Add, Sub, Mul, Div)
  , Value (Value)
  )
import MiniLang.Parser (Parser, number, char)
import Prelude hiding (div)

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

mul :: Parser Expr
mul = do
  v <- value
  char '*'
  Mul v <$> value

div :: Parser Expr
div = do
  v <- value
  char '/'
  Div v <$> value

expr :: Parser Expr
expr = add <|> sub <|> mul <|> div
