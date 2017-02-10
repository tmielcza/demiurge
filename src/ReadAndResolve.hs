module ReadAndResolve where

import Parse(parse, parseInit)
import BackwardChaining(launchResolution)
import Types
import Interactive(askForChange)

parseFile :: String -> IO (Either String ([Relation], Init, Query))
parseFile path = do
  content <- readFile (path)
  return (parse content)

readAndResolve :: String -> IO ()
readAndResolve filename= do
  parsed <- parseFile filename
  print $ do {parsed >>= launchResolution}

interactiveMode:: String -> IO ()
interactiveMode filename = do
  parsed <- parseFile filename
  print $ do {parsed >>= launchResolution}
  askForChange parsed
  return ()
