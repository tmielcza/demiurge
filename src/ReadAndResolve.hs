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
      displayEitherFactStates ret
     Left error -> do { print error; return () }


