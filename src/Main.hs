import ReadAndResolve
import System.Environment

main = do
  args <- getArgs
  if ((length args) == 1) && ((args !! 0) /= "i")
    then do
      _ <- readAndResolve (args !! 0)
      return ()
  else if (length args) == 2 && (args !! 0) == "i"
    then
      interactiveMode (args !! 1)
  else do
      print "ExpertSystem needs an argument"




