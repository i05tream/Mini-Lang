{-# LANGUAGE LambdaCase #-}

module MiniLang.Parser where

newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) = p

-- 先頭1文字を取り出すパーサ
item :: Parser Char
item = Parser $ \case
                  (c : cs) -> Just (c, cs)
                  _ -> Nothing

-- 条件に合う1文字を取り出すパーサ
match :: (Char -> Bool) -> Parser Char
match p = Parser $ \(c : cs) -> if p c then Just (c, cs) else Nothing

-- 文字を与えるとその1文字を解析するパーサを返す関数
char :: Char -> Parser Char
char c = Parser $ \case
                    (c' : cs') | c == c' -> Just (c, cs')
                    _ -> Nothing

instance Functor Parser where
  fmap f (Parser p) = Parser $ \cs ->
    case p cs of
      Just (v, cs') -> Just (f v, cs')
      _ -> Nothing

instance Applicative Parser where
  pure v = Parser $ \cs -> Just (v, cs)
