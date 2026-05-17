module Regex.Parser
  ( parseRegex
  , regexParser
  ) where

import Data.Char (isSpace)
import Regex.Syntax (Regex(..), mkAlt, mkConcat, mkStar)
import Text.Parsec
  ( ParseError
  , anyChar
  , char
  , choice
  , eof
  , many
  , many1
  , oneOf
  , parse
  , satisfy
  , skipMany
  , (<?>)
  )
import Text.Parsec.String (Parser)

parseRegex :: String -> Either ParseError Regex
parseRegex = parse regexParser "<regex>"

regexParser :: Parser Regex
regexParser = whitespace *> alternation <* eof

alternation :: Parser Regex
alternation = do
  firstPart <- concatenation
  rest <- many (symbol '|' *> concatenation)
  pure (mkAlt (firstPart : rest))

concatenation :: Parser Regex
concatenation = do
  parts <- many1 postfix
  pure (mkConcat parts)

postfix :: Parser Regex
postfix = do
  atomRegex <- atom
  operators <- many (lexeme (oneOf "*+?"))
  pure (foldl applyPostfix atomRegex operators)

applyPostfix :: Regex -> Char -> Regex
applyPostfix regex operator =
  case operator of
    '*' -> mkStar regex
    '+' -> mkConcat [regex, mkStar regex]
    '?' -> mkAlt [Epsilon, regex]
    _ -> regex

atom :: Parser Regex
atom =
  choice
    [ parenthesized
    , epsilon
    , emptySet
    , escapedLiteral
    , plainLiteral
    ]
    <?> "literal, parenthesized expression, epsilon, or empty set"

parenthesized :: Parser Regex
parenthesized = symbol '(' *> alternation <* symbol ')'

epsilon :: Parser Regex
epsilon = Epsilon <$ symbolCode 949

emptySet :: Parser Regex
emptySet = EmptySet <$ symbolCode 8709

escapedLiteral :: Parser Regex
escapedLiteral = lexeme $ do
  _ <- char '\\'
  Literal <$> anyChar

plainLiteral :: Parser Regex
plainLiteral =
  lexeme
    ( Literal
        <$> satisfy isPlainLiteral
        <?> "literal"
    )

-- Неэкранированные операторы остаются служебными. После обратной косой черты
-- любой из них снова становится обычным символом языка.
isPlainLiteral :: Char -> Bool
isPlainLiteral ch =
  not (isSpace ch)
    && ch `notElem` reservedChars
    && ch /= codeToChar 949
    && ch /= codeToChar 8709

reservedChars :: [Char]
reservedChars = "()|*+?\\"

symbol :: Char -> Parser Char
symbol ch = lexeme (char ch)

symbolCode :: Int -> Parser Char
symbolCode n = lexeme (char (codeToChar n))

lexeme :: Parser a -> Parser a
lexeme parser = parser <* whitespace

whitespace :: Parser ()
whitespace = skipMany (satisfy isSpace)

codeToChar :: Int -> Char
codeToChar = toEnum
