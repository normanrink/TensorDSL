
import Control.Exception
import System.Environment
import System.IO

import qualified Check
import qualified Parse


readFromStdin :: IO String
readFromStdin = do
  putStrLn "Reading from 'stdin' ..."
  hGetContents stdin
  
main = do
  args <- getArgs
  src <- if length args == 0
            then readFromStdin
            else readFile $ head args
  let ast = Parse.parseProgram src
  let ok  = Check.checkProgram ast
  putStrLn $ show ast ++ "\n"
  if not ok then putStrLn "There were semantic errors."
            else return ()
          
