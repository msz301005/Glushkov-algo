module Automata.DFA
  ( DFA(..)
  , State
  , acceptsDFA
  , determinize
  , dfaFromRegex
  , renderDFA
  ) where

import Automata.Glushkov (GlushkovNFA(..), buildGlushkovNFA)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Regex.Syntax (Regex)

type State = Int

data DFA = DFA
  { dfaStates :: Set State
  , dfaAlphabet :: Set Char
  , dfaStart :: State
  , dfaAccepting :: Set State
  , dfaTransitions :: Map (State, Char) State
  , dfaStateSets :: Map State (Set Int)
  } deriving (Eq, Show)

determinize :: GlushkovNFA -> DFA
determinize nfa =
  -- Алгоритм подмножеств начинается с {0}: до чтения входа позиционный автомат
  -- находится ровно в своем искусственном начальном состоянии.
  let startSet = Set.singleton (nfaStart nfa)
      initialWork = [(0, startSet)]
      initialSeen = Map.singleton startSet 0
      (seen, transitions) = explore initialSeen initialWork Map.empty 1
      stateSets = Map.fromList [(state, subset) | (subset, state) <- Map.toList seen]
      accepting =
        Set.fromList
          [ state
          | (state, subset) <- Map.toList stateSets
          , not (Set.null (Set.intersection subset (nfaAccepting nfa)))
          ]
   in DFA
        { dfaStates = Set.fromList (Map.elems seen)
        , dfaAlphabet = nfaAlphabet nfa
        , dfaStart = 0
        , dfaAccepting = accepting
        , dfaTransitions = transitions
        , dfaStateSets = stateSets
        }
  where
    explore seen work transitions nextState =
      case work of
        [] -> (seen, transitions)
        (state, subset) : rest ->
          let (seenAfter, restAfter, transitionsAfter, nextAfter) =
                foldl
                  (processSymbol state subset)
                  (seen, rest, transitions, nextState)
                  (Set.toList (nfaAlphabet nfa))
           in explore seenAfter restAfter transitionsAfter nextAfter

    processSymbol state subset (seen, work, transitions, nextState) ch =
      let target = move subset ch
       in if Set.null target
            then (seen, work, transitions, nextState)
            else
              case Map.lookup target seen of
                Just targetState ->
                  ( seen
                  , work
                  , Map.insert (state, ch) targetState transitions
                  , nextState
                  )
                Nothing ->
                  let targetState = nextState
                   in ( Map.insert target targetState seen
                      , work ++ [(targetState, target)]
                      , Map.insert (state, ch) targetState transitions
                      , nextState + 1
                      )

    move subset ch =
      Set.unions
        [ targets
        | q <- Set.toList subset
        , targets <- maybeToList (Map.lookup (q, ch) (nfaTransitions nfa))
        ]

dfaFromRegex :: Regex -> DFA
dfaFromRegex = determinize . buildGlushkovNFA

acceptsDFA :: DFA -> String -> Bool
acceptsDFA dfa input =
  case runFrom (dfaStart dfa) input of
    Nothing -> False
    Just state -> state `Set.member` dfaAccepting dfa
  where
    runFrom state chars =
      case chars of
        [] -> Just state
        ch : rest ->
          -- Построенный ДКА намеренно частичный. Если перехода нет, слово уже
          -- вышло из языка, поэтому проверка сразу отвергает его.
          case Map.lookup (state, ch) (dfaTransitions dfa) of
            Nothing -> Nothing
            Just next -> runFrom next rest

renderDFA :: DFA -> String
renderDFA dfa =
  unlines
    [ "states: " ++ renderSet (dfaStates dfa)
    , "start: " ++ show (dfaStart dfa)
    , "accepting: " ++ renderSet (dfaAccepting dfa)
    , "alphabet: " ++ renderCharSet (dfaAlphabet dfa)
    , "state sets:"
    , renderStateSets (dfaStateSets dfa)
    , "transitions:"
    , renderDfaTransitions (dfaTransitions dfa)
    ]

renderSet :: Set Int -> String
renderSet values =
  "{" ++ intercalate ", " (map show (Set.toList values)) ++ "}"

renderCharSet :: Set Char -> String
renderCharSet values =
  "{" ++ intercalate ", " (map show (Set.toList values)) ++ "}"

renderStateSets :: Map State (Set Int) -> String
renderStateSets stateSets
  | Map.null stateSets = "  {}"
  | otherwise = intercalate "\n" (map renderStateSet (Map.toList stateSets))
  where
    renderStateSet (state, subset) = "  q" ++ show state ++ " = " ++ renderSet subset

renderDfaTransitions :: Map (State, Char) State -> String
renderDfaTransitions transitions
  | Map.null transitions = "  {}"
  | otherwise = intercalate "\n" (map renderTransition (Map.toList transitions))
  where
    renderTransition ((from, ch), to) =
      "  q" ++ show from ++ " --" ++ show ch ++ "--> q" ++ show to

maybeToList :: Maybe a -> [a]
maybeToList value =
  case value of
    Nothing -> []
    Just x -> [x]
