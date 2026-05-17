module Regex.Linearize
  ( LinearSymbol(..)
  , Linearized(..)
  , Position
  , PositionedRegex(..)
  , linearize
  , renderLinearized
  ) where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Regex.Syntax (Regex(..))

type Position = Int

data LinearSymbol = LinearSymbol
  { linearPosition :: Position
  , linearCharacter :: Char
  } deriving (Eq, Ord, Show)

data PositionedRegex
  = PEmptySet
  | PEpsilon
  | PLiteral LinearSymbol
  | PConcat [PositionedRegex]
  | PAlt [PositionedRegex]
  | PStar PositionedRegex
  deriving (Eq, Show)

data Linearized = Linearized
  { positionedRegex :: PositionedRegex
  , linearSymbols :: [LinearSymbol]
  , symbolTable :: Map Position Char
  } deriving (Eq, Show)

linearize :: Regex -> Linearized
linearize regex =
  let (tree, _, symbols) = walk 1 regex
   in Linearized
        { positionedRegex = tree
        , linearSymbols = symbols
        , symbolTable = Map.fromList [(linearPosition s, linearCharacter s) | s <- symbols]
        }

walk :: Position -> Regex -> (PositionedRegex, Position, [LinearSymbol])
walk next regex =
  case regex of
    EmptySet -> (PEmptySet, next, [])
    Epsilon -> (PEpsilon, next, [])
    Literal ch ->
      let symbol = LinearSymbol next ch
       in (PLiteral symbol, next + 1, [symbol])
    Concat parts ->
      let (trees, after, symbols) = walkMany next parts
       in (PConcat trees, after, symbols)
    Alt parts ->
      let (trees, after, symbols) = walkMany next parts
       in (PAlt trees, after, symbols)
    Star inner ->
      let (tree, after, symbols) = walk next inner
       in (PStar tree, after, symbols)

walkMany :: Position -> [Regex] -> ([PositionedRegex], Position, [LinearSymbol])
walkMany next parts =
  case parts of
    [] -> ([], next, [])
    part : rest ->
      let (tree, afterPart, symbolsHere) = walk next part
          (trees, afterRest, symbolsRest) = walkMany afterPart rest
       in (tree : trees, afterRest, symbolsHere ++ symbolsRest)

renderLinearized :: Linearized -> String
renderLinearized linearized =
  case linearSymbols linearized of
    [] -> "(no literal positions)"
    symbols -> intercalate "\n" (map renderSymbol symbols)

renderSymbol :: LinearSymbol -> String
renderSymbol symbol =
  show (linearPosition symbol) ++ " -> " ++ show (linearCharacter symbol)
