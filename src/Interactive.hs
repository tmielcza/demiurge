module Interactive where

import Parse(parseInit)
import BackwardChaining(launchResolution)
import Types

getInteractiveFacts:: Either String ([Relation], Init, Query) -> IO ()
getInteractiveFacts triple = do
  print "The previous facts will be removed, please write your fact like in this example : \"=A!CF\""
  rep <- getLine
  print $ do {triple >>= (parseAndResolveInteractive rep)}
  askForChange triple

parseAndResolveInteractive :: String -> ([Relation], Init, Query) -> Either String [FactState]
parseAndResolveInteractive datas (rules, init, query) = do
  let triple = fmap (\x -> (rules, x, query)) (parseInit datas)
  triple >>= launchResolution

askForChange:: Either String ([Relation], Init, Query) -> IO()
askForChange parsed = do
  print "Do you want to change the initial facts given? (y/n)"
  rep <- getLine
  if rep == "y"
  then getInteractiveFacts parsed
  else print "Bye"; return ()
