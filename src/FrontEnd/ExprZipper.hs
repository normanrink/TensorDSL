
module ExprZipper ( ExprZipper
                  , goLeft, goRight, goUp, goDown
                  , makeZipper, expr, top, root
                  , modify ) where

import qualified AST


data ExprCtxF a = PlusLeft  { right :: a } | PlusRight  { left :: a }
                | MinusLeft { right :: a } | MinusRight { left :: a }
                | StarLeft  { right :: a } | StarRight  { left :: a }
                | SlashLeft { right :: a } | SlashRight { left :: a }
                | HashLeft  { right :: a } | HashRight  { left :: a }
                | PeriodDown { pair :: AST.Pair }
                | CaretDown  { pair :: AST.Pair }
                deriving (Eq, Show)

instance Functor ExprCtxF where
  fmap f (PlusLeft a)   = PlusLeft (f a)
  fmap f (PlusRight a)  = PlusRight (f a)
  fmap f (StarLeft a)   = StarLeft (f a)
  fmap f (StarRight a)  = StarRight (f a)
  fmap f (PeriodDown p) = PeriodDown p
  fmap f (CaretDown p)  = CaretDown p
  
type ExprLocF a = [ExprCtxF a]

goLeftF :: (AST.ExprF a, ExprLocF a) -> (a, ExprLocF a)
goLeftF (AST.Plus e0 e1, ls)  = (e0, (PlusLeft e1):ls)
goLeftF (AST.Minus e0 e1, ls) = (e0, (MinusLeft e1):ls)
goLeftF (AST.Star e0 e1, ls)  = (e0, (StarLeft e1):ls)
goLeftF (AST.Slash e0 e1, ls) = (e0, (SlashLeft e1):ls)
goLeftF (AST.Hash e0 e1, ls)  = (e0, (HashLeft e1):ls)

goRightF :: (AST.ExprF a, ExprLocF a) -> (a, ExprLocF a)
goRightF (AST.Plus e0 e1, ls)  = (e1, (PlusRight e0):ls)
goRightF (AST.Minus e0 e1, ls) = (e1, (MinusRight e0):ls)
goRightF (AST.Star e0 e1, ls)  = (e1, (StarRight e0):ls)
goRightF (AST.Slash e0 e1, ls) = (e1, (SlashRight e0):ls)
goRightF (AST.Hash e0 e1, ls)  = (e1, (HashRight e0):ls)

goDownF :: (AST.ExprF a, ExprLocF a) -> (a, ExprLocF a)
goDownF (AST.Period e p, ls) = (e, (PeriodDown p):ls)
goDownF (AST.Caret e p, ls)  = (e, (CaretDown p):ls)

goUpF :: (a, ExprLocF a) -> (AST.ExprF a, ExprLocF a)
goUpF (e, (PlusLeft  e1):ls)  = (AST.Plus e e1, ls)
goUpF (e, (PlusRight e0):ls)  = (AST.Plus e0 e, ls)
goUpF (e, (MinusLeft  e1):ls) = (AST.Minus e e1, ls)
goUpF (e, (MinusRight e0):ls) = (AST.Minus e0 e, ls)
goUpF (e, (StarLeft  e1):ls)  = (AST.Star e e1, ls)
goUpF (e, (StarRight e0):ls)  = (AST.Star e0 e, ls)
goUpF (e, (SlashLeft  e1):ls) = (AST.Slash e e1, ls)
goUpF (e, (SlashRight e0):ls) = (AST.Slash e0 e, ls)
goUpF (e, (HashLeft  e1):ls)  = (AST.Hash e e1, ls)
goUpF (e, (HashRight e0):ls)  = (AST.Hash e0 e, ls)
goUpF (e, (PeriodDown p):ls)  = (AST.Period e p, ls)
goUpF (e, (CaretDown p):ls)   = (AST.Caret e p, ls)


type ExprLoc = ExprLocF AST.Expr

type ExprZipper = (AST.Expr, ExprLoc)

makeZipper :: AST.Expr -> ExprZipper
makeZipper e = (e, [])

expr :: ExprZipper -> AST.Expr
expr = fst

goLeft :: (AST.Expr, ExprLoc) -> (AST.Expr, ExprLoc)
goLeft (e, ls) = goLeftF (AST.outE e, ls)

goRight :: (AST.Expr, ExprLoc) -> (AST.Expr, ExprLoc)
goRight (e, ls) = goRightF (AST.outE e, ls)

goDown :: (AST.Expr, ExprLoc) -> (AST.Expr, ExprLoc)
goDown (e, ls) = goDownF (AST.outE e, ls)

goUp :: (AST.Expr, ExprLoc) -> (AST.Expr, ExprLoc)
goUp (e, ls) = let (e', ls') = goUpF (e, ls)
               in (AST.InE e', ls')

loc :: ExprZipper -> ExprLocF AST.Expr
loc = snd

top :: ExprZipper -> ExprZipper
top (e, []) = (e, [])
top ez = top (goUp ez)

root :: ExprZipper -> AST.Expr
root ez = expr $ top ez

modify :: (AST.Expr -> AST.Expr) -> ExprZipper -> ExprZipper
modify f (e, ls) = (f e, ls)




