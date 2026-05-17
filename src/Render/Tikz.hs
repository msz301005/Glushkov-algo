module Render.Tikz
  ( renderStandaloneTikz
  , renderTikz
  , renderTikzPicture
  , tikzFromRegex
  ) where

import Automata.DFA (DFA(..), State, dfaFromRegex)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Regex.Syntax (Regex)

renderStandaloneTikz :: DFA -> String
renderStandaloneTikz dfa =
  unlines
    ( [ "\\documentclass[tikz,border=8pt]{standalone}"
      , "\\usetikzlibrary{automata,positioning}"
      , "\\begin{document}"
      ]
        ++ renderStateSetComments dfa
        ++ [renderTikzPicture dfa, "\\end{document}"]
    )

renderTikzPicture :: DFA -> String
renderTikzPicture dfa =
  unlines
    ( [ "\\begin{tikzpicture}[shorten >=1pt, node distance=3cm, on grid, auto]"
      ]
        ++ map (renderNode dfa) (layoutStates dfa)
        ++ renderPathBlock dfa
        ++ [ "\\end{tikzpicture}" ]
    )

tikzFromRegex :: Regex -> String
tikzFromRegex = renderStandaloneTikz . dfaFromRegex

renderTikz :: DFA -> String
renderTikz = renderStandaloneTikz

renderStateSetComments :: DFA -> [String]
renderStateSetComments dfa =
  [ "% q" ++ show state ++ " = " ++ renderIntSet subset
  | (state, subset) <- Map.toList (dfaStateSets dfa)
  ]

layoutStates :: DFA -> [(State, Int, Int)]
layoutStates dfa =
  [ (state, 3 * column, -2 * row)
  | (index, state) <- zip [0 :: Int ..] (Set.toList (dfaStates dfa))
  , let (row, column) = index `divMod` statesPerRow
  ]

statesPerRow :: Int
statesPerRow = 5

renderNode :: DFA -> (State, Int, Int) -> String
renderNode dfa (state, x, y) =
  "  \\node["
    ++ intercalate ", " (nodeStyles dfa state)
    ++ "] (q"
    ++ show state
    ++ ") at ("
    ++ show x
    ++ ","
    ++ show y
    ++ ") {$q_"
    ++ show state
    ++ "$};"

nodeStyles :: DFA -> State -> [String]
nodeStyles dfa state =
  "state" : startStyle ++ acceptingStyle
  where
    startStyle =
      if state == dfaStart dfa
        then ["initial"]
        else []
    acceptingStyle =
      if state `Set.member` dfaAccepting dfa
        then ["accepting"]
        else []

renderPathBlock :: DFA -> [String]
renderPathBlock dfa
  | Map.null grouped = []
  | otherwise =
      ["  \\path[->]"]
        ++ map renderEdge (Map.toList grouped)
        ++ ["  ;"]
  where
    grouped = groupTransitions (dfaTransitions dfa)

groupTransitions :: Map (State, Char) State -> Map (State, State) (Set Char)
groupTransitions transitions =
  Map.fromListWith Set.union
    [ ((from, to), Set.singleton ch)
    | ((from, ch), to) <- Map.toList transitions
    ]

renderEdge :: ((State, State), Set Char) -> String
renderEdge ((from, to), labels) =
  "    (q"
    ++ show from
    ++ ") edge "
    ++ edgeStyle from to
    ++ " node {"
    ++ renderLabels labels
    ++ "} (q"
    ++ show to
    ++ ")"

edgeStyle :: State -> State -> String
edgeStyle from to =
  if from == to
    then "[loop above]"
    else "[bend left=15]"

renderLabels :: Set Char -> String
renderLabels labels =
  "\\texttt{" ++ intercalate "," (map escapeTikzChar (Set.toList labels)) ++ "}"

-- Метки попадают внутрь LaTeX, поэтому безопасные для Haskell литералы иногда
-- все равно нужно экранировать перед выводом на ребро TikZ.
escapeTikzChar :: Char -> String
escapeTikzChar ch =
  case ch of
    '\\' -> "\\textbackslash{}"
    '{' -> "\\{"
    '}' -> "\\}"
    '$' -> "\\$"
    '%' -> "\\%"
    '#' -> "\\#"
    '_' -> "\\_"
    '&' -> "\\&"
    '^' -> "\\^{}"
    '~' -> "\\~{}"
    _ -> [ch]

renderIntSet :: Set Int -> String
renderIntSet values =
  "{" ++ intercalate ", " (map show (Set.toList values)) ++ "}"
