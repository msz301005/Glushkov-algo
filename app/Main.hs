module Main (main) where

import Automata.DFA (acceptsDFA, dfaFromRegex, renderDFA)
import Automata.Glushkov (attrsForRegex, buildGlushkovNFA, renderAttrs, renderNFA)
import Regex.Linearize (renderLinearized)
import Regex.Parser (parseRegex)
import Regex.Syntax (Regex, renderRegex)
import Render.Tikz (renderStandaloneTikz, renderTikzPicture)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Command
  = PrintAst String
  | PrintAttrs String
  | PrintNFA String
  | PrintDFA String
  | PrintTikz String
  | PrintTikzSnippet String
  | Match String String
  | PrintHelp

main :: IO ()
main = do
  args <- getArgs
  command <- parseCommand args
  runCommand command

parseCommand :: [String] -> IO Command
parseCommand args =
  case args of
    [] -> promptRegex PrintTikz
    ["--help"] -> pure PrintHelp
    ["-h"] -> pure PrintHelp
    "--ast" : rest -> oneRegexArgument "--ast" PrintAst rest
    "--attrs" : rest -> oneRegexArgument "--attrs" PrintAttrs rest
    "--nfa" : rest -> oneRegexArgument "--nfa" PrintNFA rest
    "--dfa" : rest -> oneRegexArgument "--dfa" PrintDFA rest
    "--tikz" : rest -> oneRegexArgument "--tikz" PrintTikz rest
    "--tikz-snippet" : rest -> oneRegexArgument "--tikz-snippet" PrintTikzSnippet rest
    ["--match", regex, word] -> pure (Match regex word)
    "--match" : _ -> matchArityFailure
    flag : _
      | looksLikeFlag flag -> usageFailureWith ("glushkov-algo: unknown option " ++ show flag ++ ".")
    [regex] -> pure (PrintTikz regex)
    _ -> usageFailureWith "glushkov-algo: REGEX without a flag expects exactly one argument."

oneRegexArgument :: String -> (String -> Command) -> [String] -> IO Command
oneRegexArgument flag build values =
  case values of
    [regex] -> pure (build regex)
    _ ->
      usageFailureWith
        ( unlines
            [ "glushkov-algo: " ++ flag ++ " expects exactly one REGEX argument."
            , "Use --match REGEX WORD to check a word."
            ]
        )

matchArityFailure :: IO Command
matchArityFailure =
  usageFailureWith "glushkov-algo: --match expects exactly REGEX and WORD arguments."

promptRegex :: (String -> Command) -> IO Command
promptRegex build = do
  putStrLn "Enter a regular expression:"
  regex <- getLine
  pure (build regex)

looksLikeFlag :: String -> Bool
looksLikeFlag value =
  case value of
    '-' : '-' : _ -> True
    _ -> False

usageFailureWith :: String -> IO Command
usageFailureWith message = do
  hPutStrLn stderr message
  hPutStrLn stderr usageText
  exitFailure

runCommand :: Command -> IO ()
runCommand command =
  case command of
    PrintHelp ->
      putStrLn usageText
    PrintAst input ->
      withRegex input $ \regex -> do
        putStrLn "Normalized AST:"
        putStrLn (renderRegex regex)
    PrintAttrs input ->
      withRegex input $ \regex -> do
        let (linearized, attrs) = attrsForRegex regex
        putStrLn "Positioned symbols:"
        putStrLn (renderLinearized linearized)
        putStrLn (renderAttrs attrs)
    PrintNFA input ->
      withRegex input $ \regex ->
        putStrLn (renderNFA (buildGlushkovNFA regex))
    PrintDFA input ->
      withRegex input $ \regex ->
        putStrLn (renderDFA (dfaFromRegex regex))
    PrintTikz input ->
      withRegex input $ \regex ->
        putStr (renderStandaloneTikz (dfaFromRegex regex))
    PrintTikzSnippet input ->
      withRegex input $ \regex ->
        putStr (renderTikzPicture (dfaFromRegex regex))
    Match input word ->
      withRegex input $ \regex ->
        putStrLn
          ( if acceptsDFA (dfaFromRegex regex) word
              then "accept"
              else "reject"
          )

withRegex :: String -> (Regex -> IO ()) -> IO ()
withRegex input action =
  case parseRegex input of
    Left err -> do
      putStrLn "Could not parse regular expression:"
      print err
      exitFailure
    Right regex -> action regex

usageText :: String
usageText =
  unlines
    [ "Usage:"
    , "  glushkov-algo REGEX"
    , "  glushkov-algo --tikz REGEX"
    , "  glushkov-algo --tikz-snippet REGEX"
    , "  glushkov-algo --ast REGEX"
    , "  glushkov-algo --attrs REGEX"
    , "  glushkov-algo --nfa REGEX"
    , "  glushkov-algo --dfa REGEX"
    , "  glushkov-algo --match REGEX WORD"
    , ""
    , "Examples:"
    , "  cabal run -v0 glushkov-algo -- --tikz \"(a|b)*abb\" > /tmp/dfa.tex"
    , "  cabal run -v0 glushkov-algo -- --match \"(a|b)*abb\" \"aabb\""
    , "  cabal run -v0 glushkov-algo -- --dfa \"(a|b)*abb\""
    ]
