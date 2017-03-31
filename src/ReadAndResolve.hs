module ReadAndResolve where

import Parse(parse)
import BackwardChaining(resolve)
import ReasoningVisualisation(showFactResolution)
import Types
import Data.Map as M(lookup)
import Debug.Trace
import Interactive(askForChange)

parseFile :: String -> Bool -> IO (Either String ([Relation], Init, Query))
parseFile path isVerbose = do
  content <- readFile (path)
  if isVerbose then do
    putStrLn "___FILE____________"
    putStrLn content
    putStrLn "___RESOLUTION______"
    return (parse content)
  else
    return (parse content)

readAndResolve :: String -> Bool -> IO (String)
readAndResolve filename isVerbose = do
  parsedData <- parseFile filename isVerbose
  let resolution = parsedData >>= resolve isVerbose
  let str = either (\err -> err) (\(_, _, q) -> resolutionToStr isVerbose resolution q) parsedData
  putStrLn str;
  return (str)

interactiveMode:: String -> Bool -> IO ()
interactiveMode filename isVerbose = do
  parsed <- parseFile filename isVerbose
  let printRes resolution parsedData = putStrLn $ either (\err -> err) (\(_, _, q) -> resolutionToStr isVerbose resolution q) parsedData
  printRes (parsed >>= resolve isVerbose) parsed
  askForChange parsed
  return ()

{-
  show resolution for each query depending
  on whether there is the option verbose
-}

resolutionToStr :: Bool ->  Either String Knowledge -> Query -> String
resolutionToStr _ (Left err) _ = err
resolutionToStr Prelude.False (Right k) qs = noVerboseResToStr k qs
resolutionToStr Prelude.True (Right k) qs = verboseResToStr k qs

noVerboseResToStr :: Knowledge -> Query -> String
noVerboseResToStr k qs =
  let
    showResolve prev (Fact q) =
      case M.lookup q k of
        Nothing -> prev ++ "error: the fact " ++ q ++ " hasn't been searched"
        Just (state, _proof) -> prev ++ "The fact " ++ q ++ " is " ++ (show state) ++ "\n"
  in foldl showResolve "" qs

verboseResToStr :: Knowledge -> Query -> String
verboseResToStr startK qs =
  let
    showResolve (prev, k) q =
      let (str, nxtK) = showFactResolution k q
      in (prev ++ str, nxtK)
  in fst (foldl showResolve ("", startK) qs)
