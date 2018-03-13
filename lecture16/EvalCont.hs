module EvalCont where

data Exp  =  Lit Double
          |  Add Exp Exp
          |  Mul Exp Exp
  deriving(Show)

type Cont a = Double -> a

evalCont :: Exp -> Cont a -> a
evalCont (Lit n) k = k n
evalCont (Add e1 e2) k =
    evalCont e1 (\v1 -> evalCont e2 (\v2 -> k (v1 + v2)))
evalCont (Mul e1 e2) k =
    evalCont e1 (\v1 -> evalCont e2 (\v2 -> k (v1 * v2)))

evalCont' :: Exp -> Cont a -> a
evalCont' (Lit n)      k =  k n
evalCont' (Add e1 e2)  k =  evalCont' e1 $ \v1 ->
                            evalCont' e2 $ \v2 ->
                            k (v1 + v2)
evalCont' (Mul e1 e2)  k =  evalCont' e1 $ \v1 ->
                            evalCont' e2 $ \v2 ->
                            k (v1 * v2)
