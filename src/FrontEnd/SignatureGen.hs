
module SignatureGen ( prettySymbol
                    , prettySignature', prettySignature
                    , prettyCHeader', prettyCHeader ) where

import Text.PrettyPrint

import qualified Context
import qualified Symbol
import qualified TType


prettySymbol :: Symbol.Symbol -> Doc
prettySymbol sym = let name  = Symbol.symbolName sym
                       ttype = Symbol.symbolType sym
                   in (text name) <> TType.prettyTType ttype

prettySignature'' :: String -> [Symbol.Symbol] -> Doc
prettySignature'' typeString syms =
  (foldl (\docs -> \d -> docs $$ (text typeString) <+> d)
         empty
         (punctuate comma (map prettySymbol syms)))

prettySignature' :: String -> Context.Context -> Doc
prettySignature' typeString ctx =
  let ins     = Context.inputs ctx
      outs    = [ s | s <- Context.outputs ctx,
                      not (s `elem` ins) ]
      insDoc  = prettySignature'' typeString ins
      outsDoc = prettySignature'' typeString outs
      allDoc  = if length ins == 0
                   then outsDoc
                   else if length outs == 0
                           then insDoc
                           else insDoc <> comma $$ outsDoc
  in parens allDoc
     
prettySignature :: String -> Context.Context -> String
prettySignature typeString ctx = render $ prettySignature' typeString ctx

prettyCHeader' :: String -> String -> String -> Context.Context -> Doc
prettyCHeader' returnType name typeString ctx =
  (text returnType) <+> (text name) <> (prettySignature' typeString ctx)

prettyCHeader :: String -> String -> String -> Context.Context -> String
prettyCHeader returnType name typeString ctx =
  render $ prettyCHeader' returnType name typeString ctx
  
