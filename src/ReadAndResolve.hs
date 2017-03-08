module ReadAndResolve where

import Parse(parse)
import BackwardChaining({-resolve,-} resolve2)
import ReasoningVisualisation(showFactResolution)
import Types
import Debug.Trace
{-import Interactive(askForChange)-}

parseFile :: String -> IO (Either String ([Relation], Init, Query))
parseFile path = do
  content <- readFile (path)
  return (parse content)

readAndResolve :: String -> IO (Either String String)
readAndResolve filename = do
  parsed <- parseFile filename
  let resolution = parsed >>= resolve2
  either putStr putStr resolution
  return (resolution)

{-
Either String ([(String, (T.State, Proof) )], String)

interactiveMode:: String -> IO ()
interactiveMode filename = do
  parsed <- parseFile filename
  print (parsed >>= resolve)
  askForChange parsed
  return ()
-}
