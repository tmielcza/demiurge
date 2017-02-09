import ReadAndResolve
import System.Environment

main = do
  args <- getArgs
  if (length args) == 0
    then
      print "ExpertSystem needs an argument"
    else do
      readAndResolve (args !! 0) >>= print




