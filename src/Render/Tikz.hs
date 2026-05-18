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
  let layout = layoutStates dfa
      coordinates = coordinateMap layout
      mainEdges = acceptingPathEdges dfa
   in
  unlines
    ( [ "\\begin{tikzpicture}[shorten >=1pt, node distance=3cm, on grid, auto]"
      ]
        ++ map (renderNode dfa) layout
        ++ renderPathBlock dfa coordinates mainEdges
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
  pathLayout ++ restLayout
  where
    path = acceptingPath dfa
    pathSet = Set.fromList path
    pathLayout =
      [ (state, 3 * column, 0)
      | (column, state) <- zip [0 :: Int ..] path
      ]
    restStates =
      filter (`Set.notMember` pathSet) (Set.toList (dfaStates dfa))
    restLayout =
      [ (state, 3 * centeredColumn rowWidth index, -3 * (row + 1))
      | (index, state) <- zip [0 :: Int ..] restStates
      , let (row, _) = index `divMod` rowWidth
      ]
    rowWidth = max 4 (length path)

centeredColumn :: Int -> Int -> Int
centeredColumn width index =
  (index + width `div` 2) `mod` width

coordinateMap :: [(State, Int, Int)] -> Map State (Int, Int)
coordinateMap layout =
  Map.fromList [(state, (x, y)) | (state, x, y) <- layout]

acceptingPathEdges :: DFA -> Set (State, State)
acceptingPathEdges dfa =
  Set.fromList (zip path (drop 1 path))
  where
    path = acceptingPath dfa

acceptingPath :: DFA -> [State]
acceptingPath dfa =
  search (Set.singleton (dfaStart dfa)) [(dfaStart dfa, [dfaStart dfa])]
  where
    search _ [] = [dfaStart dfa]
    search seen ((state, path) : rest)
      | length path > 1 && state `Set.member` dfaAccepting dfa = path
      | otherwise =
          let next =
                [ (target, path ++ [target])
                | target <- nextStates state
                , target `Set.notMember` seen
                ]
              seenAfter = Set.union seen (Set.fromList (map fst next))
           in search seenAfter (rest ++ next)

    nextStates state =
      Set.toList
        ( Set.fromList
            [ target
            | ((from, _), target) <- Map.toList (dfaTransitions dfa)
            , from == state
            ]
        )

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

renderPathBlock :: DFA -> Map State (Int, Int) -> Set (State, State) -> [String]
renderPathBlock dfa coordinates mainEdges
  | Map.null grouped = []
  | otherwise =
      ["  \\path[->]"]
        ++ map (renderEdge coordinates mainEdges) (Map.toList grouped)
        ++ ["  ;"]
  where
    grouped = groupTransitions (dfaTransitions dfa)

groupTransitions :: Map (State, Char) State -> Map (State, State) (Set Char)
groupTransitions transitions =
  Map.fromListWith Set.union
    [ ((from, to), Set.singleton ch)
    | ((from, ch), to) <- Map.toList transitions
    ]

renderEdge :: Map State (Int, Int) -> Set (State, State) -> ((State, State), Set Char) -> String
renderEdge coordinates mainEdges ((from, to), labels) =
  "    (q"
    ++ show from
    ++ ") edge "
    ++ edgeStyle coordinates mainEdges from to
    ++ " node"
    ++ labelStyle coordinates mainEdges from to
    ++ " {"
    ++ renderLabels labels
    ++ "} (q"
    ++ show to
    ++ ")"

edgeStyle :: Map State (Int, Int) -> Set (State, State) -> State -> State -> String
edgeStyle coordinates mainEdges from to
  | from == to = loopStyle coordinates from
  | (from, to) `Set.member` mainEdges = "[bend left=12]"
  | sameRow coordinates from to =
      if to > from
        then "[bend left=15]"
        else "[bend left=" ++ show (backwardBend from to) ++ "]"
  | otherwise = "[bend right=10]"

loopStyle :: Map State (Int, Int) -> State -> String
loopStyle coordinates state =
  case Map.lookup state coordinates of
    Just (_, y)
      | y < 0 -> "[loop below]"
    _ -> "[loop above]"

backwardBend :: State -> State -> Int
backwardBend from to =
  if from - to >= 3
    then 35
    else 25

labelStyle :: Map State (Int, Int) -> Set (State, State) -> State -> State -> String
labelStyle coordinates mainEdges from to
  | from == to =
      case Map.lookup from coordinates of
        Just (_, y)
          | y < 0 -> "[below]"
        _ -> "[above]"
  | (from, to) `Set.member` mainEdges = "[above]"
  | not (sameRow coordinates from to) = "[below]"
  | to < from = "[below]"
  | otherwise = "[above]"

sameRow :: Map State (Int, Int) -> State -> State -> Bool
sameRow coordinates from to =
  case (Map.lookup from coordinates, Map.lookup to coordinates) of
    (Just (_, y1), Just (_, y2)) -> y1 == y2
    _ -> True

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
