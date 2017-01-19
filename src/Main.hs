import Parse(parse)
import System.Environment

parseFile file = do
  content <- readFile ("samples/" ++ file)
  let tupleLines = parse content
      in
        return tupleLines


main = do
  args <- getArgs
  if (length args) == 0
    then
      print "ExpertSystem needs an argument"
    else
     parseFile (args !! 0) >>= print
