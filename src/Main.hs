import Parse(parse)
import BackwardChaining(launchResolution)
import System.Environment

parseFile file = do
  content <- readFile ("samples/" ++ file)
  return (parse content)



main = do
  args <- getArgs
  if (length args) == 0
    then
      print "ExpertSystem needs an argument"
    else do
     parsed <- parseFile (args !! 0)
     case parsed of
       Right triple -> print (launchResolution triple)
       Left error -> print error
