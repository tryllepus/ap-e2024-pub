module APL.Eval
  (
    Val(..), eval
  )
where

import APL.AST (Exp (..))

data Val =
  ValInt Integer
  deriving (Eq, Show)

eval :: Exp -> Val
eval (CstInt a) = ValInt a
eval (Add (CstInt a) (CstInt b)) = ValInt (a + b)
eval (Sub (CstInt a) (CstInt b)) = ValInt (a - b)
eval (Mul (CstInt a) (CstInt b)) = ValInt (a * b)
eval (Div (CstInt a) (CstInt b)) = ValInt ( a `div` b)
eval (Pow (CstInt a) (CstInt b)) = ValInt ( a ^ b)