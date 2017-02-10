module Interactive where

import Parse(parseInit)
import BackwardChaining(launchResolution)
import Types
import Data.List

getInteractiveFacts:: Either String ([Relation], Init, Query) -> IO ()
getInteractiveFacts triple = do
  displayInitDatas triple
  print "The previous facts will be removed, please write your fact like in this example : \"=A!CF\""
  rep <- getLine
  print $ do {triple >>= (parseAndResolveInteractive rep)}
  askForChange triple

parseAndResolveInteractive :: String -> ([Relation], Init, Query) -> Either String [FactState]
parseAndResolveInteractive datas (rules, init, query) = do
  let triple = fmap (\x -> (rules, refactoInitList init x, query)) (parseInit datas)
  triple >>= launchResolution

refactoInitList (Init facts)  (Init news) = Init $ unionBy (\x y-> x == y || x == Not y || Not x == y) news facts
refactoQueryList (Query facts)  (Query news) = Query (news `union` facts)

askForChange:: Either String ([Relation], Init, Query) -> IO()
askForChange parsed = do
  print "Do you want to change the initial facts given? (y/n) :"
  rep <- getLine
  if rep == "y"
  then getInteractiveFacts parsed
  else print "Bye"; return ()

displayInitDatas (Right(rls, i, q)) =
  let
      displayRule (r:[]) = print r
      displayRule (r:rs) = do { print r ; displayRule rs}
  in do { displayRule rls; print i; print q}
displayInitDatas _ = print "no datas from the previous resolution"



