
import System.Environment
import System.IO

import qualified Kernel
import qualified KernelGen
import qualified LoopIR


readFromStdin :: IO String
readFromStdin = do
  putStrLn "Reading from 'stdin' ..."
  hGetContents stdin
  
main = do
  args <- getArgs
  src <- if length args == 0
            then readFromStdin
            else readFile $ head args
  case KernelGen.fromString True src of
    Nothing -> putStrLn "There were semantic errors."
    Just k' -> let eltty = LoopIR.makeElementCType "double" "0.0"
                   code  = Kernel.printCKernel eltty "void" "kernel" k'
               in putStrLn code

                          
