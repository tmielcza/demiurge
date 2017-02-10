import ReadAndResolve
import System.Environment

main = do
  args <- getArgs
  if ((length args) == 1) && ((args !! 0) /= "i")
    then
      readAndResolve (args !! 0)
  else do
      print "ExpertSystem needs an argument"




