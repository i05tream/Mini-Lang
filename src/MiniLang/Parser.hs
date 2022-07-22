{-# LANGUAGE LambdaCase #-}

module MiniLang.Parser where

newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) = p

-- 文字を与えるとその1文字を解析するパーサを返す関数
char :: Char -> Parser Char
char c = Parser $ \case
                    (c' : cs') | c == c' -> Just (c, cs')
                    _ -> Nothing