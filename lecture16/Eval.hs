module Eval where

data Exp  =  Lit Double
          |  Add Exp Exp
          |  Mul Exp Exp
  deriving(Eq, Ord, Read, Show)

eval :: Exp -> Double
eval (Lit n)      = n
eval (Add e1 e2)  = eval e1  +  eval e2
eval (Mul e1 e2)  = eval e1  *  eval e2

eval' :: Exp -> Double
eval' (Lit n)      =  n
eval' (Add e1 e2)  =  let  v1 = eval' e1
                           v2 = eval' e2
                      in
                        v1 + v2
eval' (Mul e1 e2)  =  let  v1 = eval' e1
                           v2 = eval' e2
                      in
                        v1 * v2
