module MiniLang.Parser.AST where

import MiniLang.Data.AST
import MiniLang.Parser (Parser(Parser), number)

value :: Parser Value
value = Value <$> number
