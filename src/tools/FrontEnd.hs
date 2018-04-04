
import Control.Exception
import System.Environment
import System.IO
import Text.PrettyPrint

import qualified Check
import qualified IRGen
import qualified LiftPeriods
import qualified LoopIR
import qualified Parse
import qualified SignatureGen


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
             let header = SignatureGen.prettyCHeader' "void"
                                                      "kernel"
                                                      "double"
                                                      ctx'
             let body   = LoopIR.printCStmt' et ir
             let kernel = header <> space <> lbrace $$ (nest 2 body) $$ rbrace
             putStrLn $ render kernel
                          
