module EvalContMonad where

import Control.Monad

data Exp = Lit Double
         | Add Exp Exp
         | Mul Exp Exp
  deriving(Eq, Ord, Read, Show)

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Monad (Cont r) where
    return x = Cont $ \k -> k x
    m >>= f  = Cont $ \k -> runCont m $ \x -> runCont (f x) k

instance Functor (Cont r) where
    fmap = liftM

instance Applicative (Cont r) where
    pure  = return
    (<*>) = ap

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

eval :: Monad m => Exp -> m Double
eval (Lit n)      = return n
eval (Add e1 e2)  = do { x <- eval e1
                       ; y <- eval e2
                       ; return (x + y)
                       }
eval (Mul e1 e2)  = do { x <- eval e1
                       ; y <- eval e2
                       ; return (x * y)
                       }

evalExp :: Exp -> Double
evalExp e = runCont (eval e) id

eval' :: Exp -> Cont r Double
eval' (Lit n)      = return n
eval' (Add e1 e2)  = do { x <- eval' e1
                        ; y <- eval' e2
                        ; return (x + y)
                        }
eval' (Mul e1 e2)  = do { x <- eval' e1
                        ; y <- do { eval' e2
                                  ; callCC $ \k -> k 10
                                  }
                        ; return (x * y)
                        }
