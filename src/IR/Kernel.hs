
module Kernel ( Kernel
              , ir, inputs, outputs
              , makeKernel
              , printCKernel', printCKernel ) where

import Text.PrettyPrint

import qualified LoopIR
import qualified Symbol
import qualified TType


data Kernel n i c = K { ir   :: LoopIR.Stmt n i c
                      , ins  :: [Symbol.Symbol]
                      , outs :: [Symbol.Symbol] }
            deriving (Eq)

inputs :: Kernel n i c -> [Symbol.Symbol]
inputs = ins

outputs :: Kernel n i c -> [Symbol.Symbol]
outputs = outs

insMinusOuts :: Kernel n i c -> [Symbol.Symbol]
insMinusOuts k = [ s | s <- ins k, not $ s `elem` (outs k) ]

outsMinusIns :: Kernel n i c -> [Symbol.Symbol]
outsMinusIns k = [ s | s <- outs k, not $ s `elem` (ins k) ]

inOuts :: Kernel n i c -> [Symbol.Symbol]
inOuts k = [ s | s <- ins k, s `elem` (outs k) ]

makeKernel :: LoopIR.Stmt n i c -> [Symbol.Symbol] -> [Symbol.Symbol] -> Kernel n i c
makeKernel = K


prettyCSymbol :: Symbol.Symbol -> Doc
prettyCSymbol sym = let name  = Symbol.symbolName sym
                        ttype = Symbol.symbolType sym
                    in (text name) <> TType.prettyCTType ttype


prettyCSignature1 :: String -> [Symbol.Symbol] -> Doc
prettyCSignature1 typeString syms =
  (foldl (\docs -> \d -> docs $$ (text typeString) <+> d)
         empty
         (punctuate comma (map prettyCSymbol syms)))

prettyCSignature' :: String -> [Symbol.Symbol] -> [Symbol.Symbol] -> Doc
prettyCSignature' typeString inputs outputs =
  let insDoc     = prettyCSignature1 ("const " ++ typeString) inputs
      outsDoc    = prettyCSignature1 typeString outputs
  in if length inputs == 0
        then outsDoc
        else if length outputs == 0
                then insDoc
                else insDoc <> comma $$ outsDoc 

prettyCSignature :: String -> [Symbol.Symbol] -> [Symbol.Symbol] -> String
prettyCSignature typeString inputs outputs =
  render $ prettyCSignature' typeString inputs outputs


prettyCHeader' :: (Show n, Show i, Show c) =>
                  LoopIR.ElementCType -> String -> String -> Kernel n i c -> Doc
prettyCHeader' eltty returnType kernelName k =
  let typeString = LoopIR.typeString eltty
      inArgs     = insMinusOuts k
      outArgs    = (inOuts k) ++ (outsMinusIns k)
      signature  = prettyCSignature' typeString inArgs outArgs
  in (text returnType) <+> (text kernelName) <> parens signature

prettyCHeader :: (Show n, Show i, Show c) =>
                  LoopIR.ElementCType -> String -> String -> Kernel n i c -> String
prettyCHeader eltty returnType kernelName k =
  render $ prettyCHeader' eltty returnType kernelName k

        
printCKernel' :: (Show n, Show i, Show c) =>
                 LoopIR.ElementCType -> String -> String -> Kernel n i c -> Doc
printCKernel' eltty returnType kernelName k =
  let header  = prettyCHeader' eltty returnType kernelName k
      body    = LoopIR.printCStmt' eltty (ir k)
  in header <> space <> lbrace $$ (nest 2 body) $$ rbrace

printCKernel :: (Show n, Show i, Show c) =>
                LoopIR.ElementCType -> String -> String -> Kernel n i c -> String
printCKernel eltty returnType kernelName k =
  render $ printCKernel' eltty returnType kernelName k

  
                            
