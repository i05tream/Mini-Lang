module MiniLang.Data.AST.Expr where

newtype Value = Value Integer deriving (Show, Eq)

data Expr = Add Value Value
          | Sub Value Value
          | Mul Value Value
          | Div Value Value
          deriving (Show, Eq)