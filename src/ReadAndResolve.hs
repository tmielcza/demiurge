module ReadAndResolve where

import Parse(parse)
import BackwardChaining(resolve)
import Types
import Interactive(askForChange)

parseFile :: String -> IO (Either String ([Relation], Init, Query))
parseFile path = do
  content <- readFile (path)
  return (parse content)

readAndResolve :: String -> IO (Either String [(String, State)])
readAndResolve filename= do
  parsed <- parseFile filename
  let ret = parsed >>= resolve
  print ret
  return (ret)

interactiveMode:: String -> IO ()
interactiveMode filename = do
  parsed <- parseFile filename
  print (parsed >>= resolve)
  askForChange parsed
  return ()
