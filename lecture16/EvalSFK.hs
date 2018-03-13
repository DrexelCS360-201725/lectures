module Eval where

import Control.Applicative (Alternative(..))
import Control.Monad

-- | Success continuation
type SK r a = a -> FK r -> r

-- | Failure continuation
type FK r = r

-- | Success and failure continuation monad
newtype SFK r a = SFK { runSFK :: FK r -> SK r a -> r }

instance Functor (SFK r) where
    fmap = liftM

instance Applicative (SFK r) where
    pure  = return
    (<*>) = ap

instance Monad (SFK r) where
    return x = SFK $ \fk sk -> sk x fk
    m >>= f  = SFK $ \fk sk ->
               runSFK m     fk  $ \x fk' ->
               runSFK (f x) fk' $ \y fk'' ->
               sk y fk''

instance Alternative (SFK r) where
    empty = mzero
    (<|>) = mplus

instance MonadPlus (SFK r) where
    mzero = SFK $ \fk _sk -> fk

    m1 `mplus` m2 = SFK $ \fk sk -> runSFK m1 (runSFK m2 fk sk) sk
