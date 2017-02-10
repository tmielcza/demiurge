module ReadAndResolve where

import Parse(parse)
import BackwardChaining(launchResolution)

parseFile path = do
  content <- readFile (path)
  return (parse content)

readAndResolve filename= do
  parsed <- parseFile filename
  case parsed of
     Right triple -> do
      let ret = launchResolution triple
      displaySimpleResolution ret
     Left error -> do { print error; return () }

displaySimpleResolution (Right((fact, status):[])) = print (show fact ++" is "++ show status)

displaySimpleResolution (Right((fact, status):rs)) = do
  print (show fact ++" is "++ show status)
  displaySimpleResolution (Right rs)

displaySimpleResolution (Left err) = print $ "Error : " ++ show err
