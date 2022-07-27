module MiniLang.Parser.AST.Expr where

import MiniLang.Data.AST.Expr

import MiniLang.Parser (Parser, number)

value :: Parser Value
value = Value <$> number