module Main (main) where

import Automata.DFA (DFA(..), acceptsDFA, dfaFromRegex)
import Automata.Glushkov (Attrs(..), GlushkovNFA(..), buildGlushkovNFA, computeAttrs)
import Data.List (isInfixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Regex.Linearize
  ( LinearSymbol(..)
  , Linearized(..)
  , Position
  , PositionedRegex
  , linearize
  )
import Regex.Parser (parseRegex)
import Regex.Syntax (Regex(..), mkAlt, mkConcat, mkStar, renderRegex)
import Render.Tikz (renderStandaloneTikz)
import System.Exit (exitFailure, exitSuccess)

data ParserTest
  = ShouldParse String Regex
  | ShouldReject String

data LinearizeTest =
  ShouldLinearize String [(Position, Char)]

data AttrTest =
  ShouldHaveAttrs String Attrs

data NFATest =
  ShouldBuildNFA String (Set.Set Int) (Set.Set Int) (Map.Map (Int, Char) (Set.Set Int))

data MatchTest =
  ShouldMatch String String Bool

data TikzTest =
  ShouldContainTikz String [String]

main :: IO ()
main = do
  parserFailures <- mapM runParserTest parserTests
  linearizeFailures <- mapM runLinearizeTest linearizeTests
  attrFailures <- mapM runAttrTest attrTests
  nfaFailures <- mapM runNFATest nfaTests
  matchFailures <- mapM runMatchTest matchTests
  tikzFailures <- mapM runTikzTest tikzTests
  let totalFailures =
        parserFailures
          ++ linearizeFailures
          ++ attrFailures
          ++ nfaFailures
          ++ matchFailures
          ++ tikzFailures
      totalTests =
        length parserTests
          + length linearizeTests
          + length attrTests
          + length nfaTests
          + length matchTests
          + length tikzTests
  if or totalFailures
    then exitFailure
    else do
      putStrLn ("All tests passed: " ++ show totalTests)
      exitSuccess

parserTests :: [ParserTest]
parserTests =
  [ ShouldParse "a" (Literal 'a')
  , ShouldParse "a b c" (mkConcat [Literal 'a', Literal 'b', Literal 'c'])
  , ShouldParse "a|b*c" (mkAlt [Literal 'a', mkConcat [mkStar (Literal 'b'), Literal 'c']])
  , ShouldParse "(a|b)*c" (mkConcat [mkStar (mkAlt [Literal 'a', Literal 'b']), Literal 'c'])
  , ShouldParse "a+" (mkConcat [Literal 'a', mkStar (Literal 'a')])
  , ShouldParse "a?" (mkAlt [Epsilon, Literal 'a'])
  , ShouldParse "\949" Epsilon
  , ShouldParse "\8709" EmptySet
  , ShouldParse "a\\|b" (mkConcat [Literal 'a', Literal '|', Literal 'b'])
  , ShouldParse "\\*" (Literal '*')
  , ShouldReject ""
  , ShouldReject "a|"
  , ShouldReject "()"
  , ShouldReject "*a"
  ]

linearizeTests :: [LinearizeTest]
linearizeTests =
  [ ShouldLinearize "a" [(1, 'a')]
  , ShouldLinearize "aa" [(1, 'a'), (2, 'a')]
  , ShouldLinearize "a(a|a)" [(1, 'a'), (2, 'a'), (3, 'a')]
  , ShouldLinearize "\\*" [(1, '*')]
  ]

attrTests :: [AttrTest]
attrTests =
  [ ShouldHaveAttrs "\949" (attrs True [] [] [])
  , ShouldHaveAttrs "\8709" (attrs False [] [] [])
  , ShouldHaveAttrs "a" (attrs False [1] [1] [])
  , ShouldHaveAttrs "ab" (attrs False [1] [2] [(1, [2])])
  , ShouldHaveAttrs "a*" (attrs True [1] [1] [(1, [1])])
  , ShouldHaveAttrs "a?b" (attrs False [1, 2] [2] [(1, [2])])
  , ShouldHaveAttrs
      "(a|b)*abb"
      (attrs False [1, 2, 3] [5] [(1, [1, 2, 3]), (2, [1, 2, 3]), (3, [4]), (4, [5])])
  ]

nfaTests :: [NFATest]
nfaTests =
  [ ShouldBuildNFA "\949" (set [0]) (set [0]) Map.empty
  , ShouldBuildNFA "\8709" (set [0]) Set.empty Map.empty
  , ShouldBuildNFA "a" (set [0, 1]) (set [1]) (transitions [(0, 'a', [1])])
  , ShouldBuildNFA "ab" (set [0, 1, 2]) (set [2]) (transitions [(0, 'a', [1]), (1, 'b', [2])])
  , ShouldBuildNFA "a*" (set [0, 1]) (set [0, 1]) (transitions [(0, 'a', [1]), (1, 'a', [1])])
  , ShouldBuildNFA "(a|a)" (set [0, 1, 2]) (set [1, 2]) (transitions [(0, 'a', [1, 2])])
  ]

matchTests :: [MatchTest]
matchTests =
  concat
    [ matches "\8709" [("", False), ("a", False)]
    , matches "\949" [("", True), ("a", False)]
    , matches "a" [("a", True), ("", False), ("aa", False), ("b", False)]
    , matches "aa" [("aa", True), ("a", False), ("aaa", False)]
    , matches "a*" [("", True), ("a", True), ("aaaa", True), ("b", False)]
    , matches "a?b" [("b", True), ("ab", True), ("", False), ("a", False), ("abb", False)]
    , matches "\\*" [("*", True), ("", False), ("a", False)]
    , matches
        "(a|b)*abb"
        [ ("abb", True)
        , ("aabb", True)
        , ("bababb", True)
        , ("", False)
        , ("ab", False)
        , ("aba", False)
        , ("abba", False)
        ]
    ]

tikzTests :: [TikzTest]
tikzTests =
  [ ShouldContainTikz
      "a"
      [ "\\documentclass"
      , "\\usetikzlibrary{automata,positioning}"
      , "\\begin{tikzpicture}"
      , "\\end{tikzpicture}"
      ]
  , ShouldContainTikz "a" ["(q0)", "(q1)", "{$q_0$}", "{$q_1$}"]
  , ShouldContainTikz "a" ["\\node[state, initial] (q0)", "\\node[state, accepting] (q1)"]
  , ShouldContainTikz "a" ["node {\\texttt{a}}"]
  , ShouldContainTikz "\949" ["\\node[state, initial, accepting] (q0)"]
  , ShouldContainTikz "a*" ["loop above"]
  , ShouldContainTikz "\\*" ["node {\\texttt{*}}"]
  , ShouldContainTikz "(a|a)" ["% q1 = {1, 2}"]
  ]

matches :: String -> [(String, Bool)] -> [MatchTest]
matches regex cases =
  [ShouldMatch regex word expected | (word, expected) <- cases]

attrs :: Bool -> [Position] -> [Position] -> [(Position, [Position])] -> Attrs
attrs canBeEmpty first lastPositions follow =
  Attrs
    { nullable = canBeEmpty
    , firstPos = set first
    , lastPos = set lastPositions
    , followPos = Map.fromList [(pos, set targets) | (pos, targets) <- follow]
    }

set :: Ord a => [a] -> Set.Set a
set = Set.fromList

transitions :: [(Int, Char, [Int])] -> Map.Map (Int, Char) (Set.Set Int)
transitions entries =
  Map.fromList [((from, ch), set targets) | (from, ch, targets) <- entries]

runParserTest :: ParserTest -> IO Bool
runParserTest test =
  case test of
    ShouldParse input expected ->
      case parseRegex input of
        Right actual
          | actual == expected -> pure False
          | otherwise -> do
              putStrLn ("FAIL parse " ++ show input)
              putStrLn ("  expected: " ++ renderRegex expected)
              putStrLn ("  actual:   " ++ renderRegex actual)
              pure True
        Left err -> do
          putStrLn ("FAIL parse " ++ show input)
          putStrLn ("  unexpected error: " ++ show err)
          pure True
    ShouldReject input ->
      case parseRegex input of
        Left _ -> pure False
        Right actual -> do
          putStrLn ("FAIL reject " ++ show input)
          putStrLn ("  parsed as: " ++ renderRegex actual)
          pure True

runLinearizeTest :: LinearizeTest -> IO Bool
runLinearizeTest (ShouldLinearize input expected) =
  case parseRegex input of
    Left err -> do
      putStrLn ("FAIL linearize " ++ show input)
      putStrLn ("  parse error: " ++ show err)
      pure True
    Right regex -> do
      let linearized = linearize regex
          actual = [(linearPosition s, linearCharacter s) | s <- linearSymbols linearized]
          expectedTable = Map.fromList expected
      if actual == expected && symbolTable linearized == expectedTable
        then pure False
        else do
          putStrLn ("FAIL linearize " ++ show input)
          putStrLn ("  expected symbols: " ++ show expected)
          putStrLn ("  actual symbols:   " ++ show actual)
          putStrLn ("  actual table:     " ++ show (symbolTable linearized))
          pure True

runAttrTest :: AttrTest -> IO Bool
runAttrTest (ShouldHaveAttrs input expected) =
  case positionedFromInput input of
    Left err -> do
      putStrLn ("FAIL attrs " ++ show input)
      putStrLn ("  parse error: " ++ show err)
      pure True
    Right positioned -> do
      let actual = computeAttrs positioned
      if actual == expected
        then pure False
        else do
          putStrLn ("FAIL attrs " ++ show input)
          putStrLn ("  expected: " ++ show expected)
          putStrLn ("  actual:   " ++ show actual)
          pure True

runNFATest :: NFATest -> IO Bool
runNFATest (ShouldBuildNFA input expectedStates expectedAccepting expectedTransitions) =
  case parseRegex input of
    Left err -> do
      putStrLn ("FAIL nfa " ++ show input)
      putStrLn ("  parse error: " ++ show err)
      pure True
    Right regex -> do
      let nfa = buildGlushkovNFA regex
      if nfaStates nfa == expectedStates
          && nfaStart nfa == 0
          && nfaAccepting nfa == expectedAccepting
          && nfaTransitions nfa == expectedTransitions
        then pure False
        else do
          putStrLn ("FAIL nfa " ++ show input)
          putStrLn ("  states:      " ++ show (nfaStates nfa))
          putStrLn ("  accepting:   " ++ show (nfaAccepting nfa))
          putStrLn ("  transitions: " ++ show (nfaTransitions nfa))
          pure True

runMatchTest :: MatchTest -> IO Bool
runMatchTest (ShouldMatch input word expected) =
  case parseRegex input of
    Left err -> do
      putStrLn ("FAIL match " ++ show input ++ " " ++ show word)
      putStrLn ("  parse error: " ++ show err)
      pure True
    Right regex -> do
      let dfa = dfaFromRegex regex
          actual = acceptsDFA dfa word
      if actual == expected && dfaLooksDeterministic dfa
        then pure False
        else do
          putStrLn ("FAIL match " ++ show input ++ " " ++ show word)
          putStrLn ("  expected: " ++ show expected)
          putStrLn ("  actual:   " ++ show actual)
          putStrLn ("  dfa:      " ++ show dfa)
          pure True

dfaLooksDeterministic :: DFA -> Bool
dfaLooksDeterministic dfa =
  all (`Set.member` dfaStates dfa) (Map.elems (dfaTransitions dfa))

runTikzTest :: TikzTest -> IO Bool
runTikzTest (ShouldContainTikz input needles) =
  case parseRegex input of
    Left err -> do
      putStrLn ("FAIL tikz " ++ show input)
      putStrLn ("  parse error: " ++ show err)
      pure True
    Right regex -> do
      let tex = renderStandaloneTikz (dfaFromRegex regex)
          missing = filter (`notIn` tex) needles
      if null missing
        then pure False
        else do
          putStrLn ("FAIL tikz " ++ show input)
          putStrLn ("  missing: " ++ show missing)
          putStrLn tex
          pure True

notIn :: String -> String -> Bool
notIn needle haystack =
  not (needle `isInfixOf` haystack)

positionedFromInput :: String -> Either String PositionedRegex
positionedFromInput input =
  case parseRegex input of
    Left err -> Left (show err)
    Right regex -> Right (positionedRegex (linearize regex))
