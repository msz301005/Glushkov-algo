module Regex.Syntax
  ( Regex(..)
  , mkAlt
  , mkConcat
  , mkStar
  , renderRegex
  ) where

import Data.List (intercalate)

data Regex
  = EmptySet
  | Epsilon
  | Literal Char
  | Concat [Regex]
  | Alt [Regex]
  | Star Regex
  deriving (Eq, Ord, Show)

-- Парсер проходит через эти вспомогательные функции, чтобы следующие этапы
-- получали компактное дерево ядра, а не вложенность вида Concat [Concat [...]].
mkConcat :: [Regex] -> Regex
mkConcat parts =
  case flatten parts of
    [] -> Epsilon
    [single] -> single
    many -> Concat many
  where
    flatten [] = []
    flatten (Concat xs : rest) = flatten xs ++ flatten rest
    flatten (x : rest) = x : flatten rest

mkAlt :: [Regex] -> Regex
mkAlt parts =
  case flatten parts of
    [] -> EmptySet
    [single] -> single
    many -> Alt many
  where
    flatten [] = []
    flatten (Alt xs : rest) = flatten xs ++ flatten rest
    flatten (x : rest) = x : flatten rest

mkStar :: Regex -> Regex
mkStar = Star

renderRegex :: Regex -> String
renderRegex regex =
  case regex of
    EmptySet -> "EmptySet"
    Epsilon -> "Epsilon"
    Literal ch -> "Literal " ++ show ch
    Concat parts -> "Concat [" ++ renderMany parts ++ "]"
    Alt parts -> "Alt [" ++ renderMany parts ++ "]"
    Star inner -> "Star (" ++ renderRegex inner ++ ")"

renderMany :: [Regex] -> String
renderMany = intercalate ", " . map renderRegex
