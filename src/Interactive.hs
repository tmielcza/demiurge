module Interactive where

import Parse(parseInit, parseQuery)
import BackwardChaining(resolve)
import Types
import Data.List()
import System.IO

askForChange:: Either String ([Relation], Init, Query) -> IO()
askForChange (Right parsed) = do
  print "Do you want to change the initial facts given? (y/n) :"
  rep <- getLine
  if rep == "y"
  then do {displayInitDatas (Right parsed);promptAddData (Right parsed); askForChange (Right parsed)}
  else print "Bye"

askForChange (Left _) = print "Their was an error in your file correct it before using interactive maode with it";

displayInitDatas (Right(rls, i, q)) =
  let
      displayRule [r] = print r
      displayRule (r:rs) = do { print r ; displayRule rs}
  in do { displayRule rls; print i; print q}
displayInitDatas _ = print "no datas from the previous resolution"


prompt :: ([Relation], Init, Query) -> IO(Either String ([Relation], Init, Query))
prompt datas = do
  putStr "$> "
  hFlush stdout
  line <- getLine
  if line == "q"
    then return (Right datas)
    else readEntry datas line

(+++) :: Either String a -> Either String a -> Either String a
Left _ +++ other = other
Right a +++ _ = Right a


readEntry :: ([Relation], Init, Query) -> String ->IO(Either String ([Relation], Init, Query))
readEntry (r, i, q) line =
  let res = fmap (\x -> (r, x, q)) (parseInit line) +++
               fmap (\x -> (r, i, x)) (parseQuery line)
  in
  case res of
    Right triple -> prompt triple
    Left err -> do {print ("input: " ++ err); prompt (r, i, q);}

promptAddData :: Either String ([Relation], Init, Query) -> IO()
promptAddData (Right triple) = do
  datas <- prompt triple
  print (datas >>= resolve)
  return ()

promptAddData _ = print "your previous file was incorrect, impossible to launch the interactive mode"



