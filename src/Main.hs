import ReadAndResolve
import System.Environment
import Control.Monad(void)

main = do
  args <- getArgs
  if (length args == 1) && (head args /= "i") then
    void (readAndResolve (head args) False)
  {-else if length args == 2 && head args == "i" then
    interactiveMode (args !! 1)-}
  else if length args == 2 && head args == "v" then
    void (readAndResolve (last args) True)

  else
    print "ExpertSystem needs an file in argument"
