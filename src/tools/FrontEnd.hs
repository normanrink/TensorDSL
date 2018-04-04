
import System.Environment
import System.IO

import qualified Check
import qualified IRGen
import qualified LiftPeriods
import qualified LoopIR
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
  let ast       = Parse.parseProgram src
  let (ok, ctx) = Check.checkProgram ast
  putStrLn $ show ast ++ "\n"
  if not ok
     then do putStrLn "There were semantic errors."
             return ()
     else do let (ast', ctx') = LiftPeriods.liftPeriodsInProgram ast ctx
             putStrLn "Program with lifted periods:"
             putStrLn $ show ast' ++ "\n"
             let ir   = IRGen.fromASTProgram ast' ctx'
             let et   = LoopIR.makeElementCType "double" "0.0"
             putStrLn "IR:"
             putStrLn $ LoopIR.printCStmt et ir

                          
