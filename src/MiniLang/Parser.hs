{-# LANGUAGE LambdaCase #-}

module MiniLang.Parser where

import Data.Char (isDigit)
import Control.Applicative (Alternative (empty, (<|>), many))

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
match p = Parser $ \case
                    (c : cs) | p c -> Just (c, cs)
                    _ -> Nothing

-- 文字を与えるとその1文字を解析するパーサを返す関数
char :: Char -> Parser Char
char c = match (== c)

nonZeroDigit :: Parser Char
nonZeroDigit = match (`elem` "123456789")

number :: Parser Integer
number =
  do
    c <- nonZeroDigit
    cs <- many . match $ isDigit
    return . read $ c : cs
  <|>
  do
    c <- match (== '0')
    return 0

instance Monad Parser where
  return v = Parser $ \cs -> Just(v, cs)

  p >>= f = Parser $ \cs -> 
    case parse p cs of
      Just (x, cs') -> parse (f x) cs'
      _ -> Nothing

instance Applicative Parser where
  pure = return

  mf <*> mx = Parser $ \cs -> do
    (f, cs')  <- parse mf cs
    (v, cs'') <- parse mx cs'
    return (f v, cs'')

instance Functor Parser where
  fmap f p = Parser $ \cs -> do
    (v, cs') <- parse p cs
    return (f v, cs')

instance Alternative Parser where
  empty = Parser $ const Nothing

  p <|> p' = Parser $ \cs ->
    case parse p cs of
      Nothing -> parse p' cs
      x -> x