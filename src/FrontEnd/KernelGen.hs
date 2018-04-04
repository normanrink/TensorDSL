
module KernelGen ( fromString,
                   fromASTProgram ) where

import qualified AST
import qualified Check
import qualified Context
import qualified IRGen
import qualified Kernel as K
import qualified LiftPeriods as LP
import qualified LoopIR
import qualified Parse


type Kernel = K.Kernel String String Int

fromASTProgram :: AST.Program -> Context.Context -> Kernel
fromASTProgram p ctx = let ir      = IRGen.fromASTProgram p ctx
                           inputs  = Context.inputs ctx
                           outputs = Context.outputs ctx
                       in K.makeKernel ir inputs outputs

fromString :: Bool -> String -> Maybe Kernel
fromString liftPeriods src =
  let ast       = Parse.parseProgram src
      (ok, ctx) = Check.checkProgram ast
  in if not ok
        then Nothing
        else let (ast', ctx') = if liftPeriods
                                   then LP.liftPeriodsInProgram ast ctx
                                   else (ast, ctx)
             in Just $ fromASTProgram ast' ctx'

