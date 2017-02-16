module ReadAndResolve where

import Parse(parse, parseInit)
import BackwardChaining(loopOnQuery)
import Types
import Interactive(askForChange)

parseFile :: String -> IO (Either String ([Relation], Init, Query))
parseFile path = do
  content <- readFile (path)
  return (parse content)

readAndResolve :: String -> IO (Either String [FactState])
readAndResolve filename= do
  parsed <- parseFile filename
  let ret = parsed >>= loopOnQuery
  print ret
  return (ret)

interactiveMode:: String -> IO ()
interactiveMode filename = do
  parsed <- parseFile filename
  print (parsed >>= loopOnQuery)
  askForChange parsed
  return ()
