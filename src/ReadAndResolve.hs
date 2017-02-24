module ReadAndResolve where

import Parse(parse)
import BackwardChaining(resolve, resolve2)
import Types
import Interactive(askForChange)

parseFile :: String -> IO (Either String ([Relation], Init, Query))
parseFile path = do
  content <- readFile (path)
  return (parse content)

readAndResolve :: String -> IO (Either String [(String, State)])
readAndResolve filename= do
  parsed <- parseFile filename
  let resolution = parsed >>= resolve2
  let ret = fmap fst resolution
  either (print ) (putStr) (fmap snd resolution)
  print (ret)
  return (ret)

interactiveMode:: String -> IO ()
interactiveMode filename = do
  parsed <- parseFile filename
  print (parsed >>= resolve)
  askForChange parsed
  return ()
