module ReadAndResolve where

import Parse(parse)
import BackwardChaining(resolve)
import ReasoningVisualisation(showFactResolution)
import Types
import Data.Map as M(lookup)
import Debug.Trace
{-import Interactive(askForChange)-}

parseFile :: String -> IO (Either String ([Relation], Init, Query))
parseFile path = do
  putStrLn "___FILE____________"
  content <- readFile (path)
  putStrLn content
  putStrLn "___RESOLUTION______"
  return (parse content)

readAndResolve :: String -> Bool -> IO (String)
readAndResolve filename isVerbose = do
  parsedData <- parseFile filename
  let resolution = parsedData >>= resolve
  let str = either (\err -> err) (\(_, _, q) -> resolutionToStr isVerbose resolution q) parsedData
  putStrLn str;
  return (str)

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
        Just (state, _proof) -> prev ++ "The fact " ++ q ++ " is " ++ (show state)
  in foldl showResolve "" qs

verboseResToStr :: Knowledge -> Query -> String
verboseResToStr startK qs =
  let
    showResolve (prev, k) q =
      let (str, nxtK) = showFactResolution k q
      in (prev ++ str, nxtK)
  in fst (foldl showResolve ("", startK) qs)


{-
Either String ([(String, (T.State, Proof) )], String)

interactiveMode:: String -> IO ()
interactiveMode filename = do
  parsed <- parseFile filename
  print (parsed >>= resolve)
  askForChange parsed
  return ()
-}
