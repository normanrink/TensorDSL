
module IRGenContext ( IRGenContext
                    , astCtx
                    , makeIRGenContext
                    , freshVariable
                    , freshIndex
                    , freshIndices ) where

import Control.Monad.State

import qualified Context


data IRGenContext = IRGC { astCtx :: Context.Context
                         , vars :: Int
                         , inds :: Int }
                    deriving (Eq, Show)

makeIRGenContext :: Context.Context -> IRGenContext
makeIRGenContext astCtx = IRGC astCtx 0 0

incVars :: IRGenContext -> IRGenContext
incVars (IRGC astCtx vars inds) = IRGC astCtx (vars+1) inds

incInds :: IRGenContext -> IRGenContext
incInds (IRGC astCtx vars inds) = IRGC astCtx vars (inds+1)

-- Note that underscores are not allowed
-- in variable names in the source language.
freshVariable :: State IRGenContext String
freshVariable = do irgc <- get
                   put $ incVars irgc
                   return $ "_v" ++ show (vars irgc)
  
freshIndex :: State IRGenContext String
freshIndex = do irgc <- get
                put $ incInds irgc
                return $ "_i" ++ show (inds irgc)

freshIndices :: Int -> State IRGenContext [String]
freshIndices n = let folder = \inds ->
                               \_ -> do { i <- freshIndex; return (i:inds) }
                 in do is <- foldM folder [] [1..n]
                       return $ reverse is

