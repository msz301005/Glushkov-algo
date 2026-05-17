module Automata.Glushkov
  ( Attrs(..)
  , GlushkovNFA(..)
  , attrsForRegex
  , buildGlushkovNFA
  , computeAttrs
  , renderAttrs
  , renderNFA
  ) where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Regex.Linearize
  ( LinearSymbol(..)
  , Linearized(..)
  , Position
  , PositionedRegex(..)
  , linearize
  , positionedRegex
  )
import Regex.Syntax (Regex)

data Attrs = Attrs
  { nullable :: Bool
  , firstPos :: Set Position
  , lastPos :: Set Position
  , followPos :: Map Position (Set Position)
  } deriving (Eq, Show)

data GlushkovNFA = GlushkovNFA
  { nfaStates :: Set Int
  , nfaStart :: Int
  , nfaAccepting :: Set Int
  , nfaTransitions :: Map (Int, Char) (Set Int)
  , nfaAlphabet :: Set Char
  , nfaStateLabels :: Map Int Char
  , nfaAttrs :: Attrs
  } deriving (Eq, Show)

attrsForRegex :: Regex -> (Linearized, Attrs)
attrsForRegex regex =
  let linearized = linearize regex
   in (linearized, computeAttrs (positionedRegex linearized))

buildGlushkovNFA :: Regex -> GlushkovNFA
buildGlushkovNFA regex =
  let (linearized, attrs) = attrsForRegex regex
      labels = symbolTable linearized
      states = Set.insert 0 (Map.keysSet labels)
      accepting =
        if nullable attrs
          then Set.insert 0 (lastPos attrs)
          else lastPos attrs
      transitions =
        mergeNfaTransitions
          [ transitionsFromStart labels (firstPos attrs)
          , transitionsFromFollow labels (followPos attrs)
          ]
   in GlushkovNFA
        { nfaStates = states
        , nfaStart = 0
        , nfaAccepting = accepting
        , nfaTransitions = transitions
        , nfaAlphabet = Set.fromList (Map.elems labels)
        , nfaStateLabels = labels
        , nfaAttrs = attrs
        }

transitionsFromStart :: Map Int Char -> Set Position -> Map (Int, Char) (Set Int)
transitionsFromStart labels starts =
  -- Метка перехода берется из целевой позиции: позиция p означает конкретное
  -- вхождение symbol(p), поэтому получается 0 --symbol(p)--> p.
  Map.fromListWith Set.union
    [ ((0, ch), Set.singleton pos)
    | pos <- Set.toList starts
    , ch <- maybeToList (Map.lookup pos labels)
    ]

transitionsFromFollow :: Map Int Char -> Map Position (Set Position) -> Map (Int, Char) (Set Int)
transitionsFromFollow labels follow =
  Map.fromListWith Set.union (concatMap transitionsForEntry (Map.toList follow))
  where
    transitionsForEntry (from, targets) =
      mapMaybe (transitionToTarget from) (Set.toList targets)
    transitionToTarget from target = do
      ch <- Map.lookup target labels
      pure ((from, ch), Set.singleton target)

maybeToList :: Maybe a -> [a]
maybeToList value =
  case value of
    Nothing -> []
    Just x -> [x]

mergeNfaTransitions :: [Map (Int, Char) (Set Int)] -> Map (Int, Char) (Set Int)
mergeNfaTransitions = foldl (Map.unionWith Set.union) Map.empty

computeAttrs :: PositionedRegex -> Attrs
computeAttrs regex =
  case regex of
    PEmptySet -> emptyAttrs
    PEpsilon -> epsilonAttrs
    PLiteral symbol -> literalAttrs (linearPosition symbol)
    PAlt parts -> foldAttrs emptyAttrs combineAlt parts
    PConcat parts -> foldAttrs epsilonAttrs combineConcat parts
    PStar inner -> starAttrs (computeAttrs inner)

foldAttrs :: Attrs -> (Attrs -> Attrs -> Attrs) -> [PositionedRegex] -> Attrs
foldAttrs neutral combine parts =
  case parts of
    [] -> neutral
    firstPart : rest -> foldl combine (computeAttrs firstPart) (map computeAttrs rest)

emptyAttrs :: Attrs
emptyAttrs =
  Attrs
    { nullable = False
    , firstPos = Set.empty
    , lastPos = Set.empty
    , followPos = Map.empty
    }

epsilonAttrs :: Attrs
epsilonAttrs =
  Attrs
    { nullable = True
    , firstPos = Set.empty
    , lastPos = Set.empty
    , followPos = Map.empty
    }

literalAttrs :: Position -> Attrs
literalAttrs pos =
  Attrs
    { nullable = False
    , firstPos = Set.singleton pos
    , lastPos = Set.singleton pos
    , followPos = Map.empty
    }

combineAlt :: Attrs -> Attrs -> Attrs
combineAlt left right =
  Attrs
    { nullable = nullable left || nullable right
    , firstPos = Set.union (firstPos left) (firstPos right)
    , lastPos = Set.union (lastPos left) (lastPos right)
    , followPos = mergeFollow (followPos left) (followPos right)
    }

combineConcat :: Attrs -> Attrs -> Attrs
combineConcat left right =
  Attrs
    { nullable = nullable left && nullable right
    , firstPos =
        if nullable left
          then Set.union (firstPos left) (firstPos right)
          else firstPos left
    , lastPos =
        if nullable right
          then Set.union (lastPos left) (lastPos right)
          else lastPos right
    , followPos =
        mergeFollows
          [ followPos left
          , followPos right
          , linksFrom (lastPos left) (firstPos right)
          ]
    }

starAttrs :: Attrs -> Attrs
starAttrs inner =
  -- В r* после любой последней позиции r снова может идти любая первая позиция
  -- r. Именно эта маленькая связь потом превращается в циклы автомата.
  Attrs
    { nullable = True
    , firstPos = firstPos inner
    , lastPos = lastPos inner
    , followPos =
        mergeFollow
          (followPos inner)
          (linksFrom (lastPos inner) (firstPos inner))
    }

linksFrom :: Set Position -> Set Position -> Map Position (Set Position)
linksFrom from to
  | Set.null to = Map.empty
  | otherwise = Map.fromSet (const to) from

mergeFollow :: Map Position (Set Position) -> Map Position (Set Position) -> Map Position (Set Position)
mergeFollow = Map.unionWith Set.union

mergeFollows :: [Map Position (Set Position)] -> Map Position (Set Position)
mergeFollows = foldl mergeFollow Map.empty

renderAttrs :: Attrs -> String
renderAttrs attrs =
  unlines
    [ "nullable: " ++ show (nullable attrs)
    , "first: " ++ renderSet (firstPos attrs)
    , "last: " ++ renderSet (lastPos attrs)
    , "follow:"
    , renderFollow (followPos attrs)
    ]

renderNFA :: GlushkovNFA -> String
renderNFA nfa =
  unlines
    [ "states: " ++ renderSet (nfaStates nfa)
    , "start: " ++ show (nfaStart nfa)
    , "accepting: " ++ renderSet (nfaAccepting nfa)
    , "alphabet: " ++ renderCharSet (nfaAlphabet nfa)
    , "labels:"
    , renderLabels (nfaStateLabels nfa)
    , "transitions:"
    , renderNfaTransitions (nfaTransitions nfa)
    ]

renderSet :: Set Int -> String
renderSet values =
  "{" ++ intercalate ", " (map show (Set.toList values)) ++ "}"

renderCharSet :: Set Char -> String
renderCharSet values =
  "{" ++ intercalate ", " (map show (Set.toList values)) ++ "}"

renderFollow :: Map Position (Set Position) -> String
renderFollow follow
  | Map.null follow = "  {}"
  | otherwise = intercalate "\n" (map renderEntry (Map.toList follow))
  where
    renderEntry (pos, values) = "  " ++ show pos ++ " -> " ++ renderSet values

renderLabels :: Map Int Char -> String
renderLabels labels
  | Map.null labels = "  {}"
  | otherwise = intercalate "\n" (map renderLabel (Map.toList labels))
  where
    renderLabel (state, ch) = "  " ++ show state ++ " = " ++ show ch

renderNfaTransitions :: Map (Int, Char) (Set Int) -> String
renderNfaTransitions transitions
  | Map.null transitions = "  {}"
  | otherwise = intercalate "\n" (map renderTransition (Map.toList transitions))
  where
    renderTransition ((from, ch), targets) =
      "  " ++ show from ++ " --" ++ show ch ++ "--> " ++ renderSet targets
