{-# OPTIONS_GHC -Wall #-}

module Examples where

import Prelude hiding (Maybe(..), Either(..))

import Control.Monad (ap)
import Data.Complex
import System.Random

--
-- Abstracting map
--
data Maybe a = Nothing
             | Just a
  deriving (Eq, Ord, Show, Read)

-- mapMaybe ::

data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Read)

-- mapTree ::

-- Functor instance for Maybe

-- instance Functor Maybe where

data Either a b = Left a
                | Right b
  deriving (Eq, Ord, Show, Read)

-- Functor instance for Either

-- instance Functor Either... where

--
-- Abstracting over computation
--

-- applyMaybe ::

-- pureToMaybe ::

--
-- Debuggable functions
--
f', g' :: Float -> (Float, String)
f' x = (x + 1, "Called f")
g' x = (x * 2, "Called g")

g'Thenf' :: Float -> (Float, String)
g'Thenf' x = let (y, s1) = g' x
                 (z, s2) = f' y
             in
               (z, s1++s2)

{-
bind :: (Float, String) -> (Float -> (Float, String)) -> (Float, String)

pure :: Float -> (Float, String)

lift :: (Float -> Float) -> Float -> (Float, String)
-}

--
-- Multivalued functions
--
sqrt', cbrt' :: Complex Float -> [Complex Float]
cbrt' = rootC 3
sqrt' = rootC 2

rootC :: Float -> Complex Float -> [Complex Float]
rootC n (a :+ b) =
    zipWith (:+) r i
  where
    r   = map (* (m ** (1/n) )) $ map cos $ map arg [0..n-1]
    i   = map (* (m ** (1/n) )) $ map sin $ map arg [0..n-1]
    arg = ( * (2*pi / n) )
    m   = sqrt (a*a + b*b)

{-
bind :: [Complex Float] -> (Complex Float -> [Complex Float]) -> [Complex Float]

pure :: Complex Float -> [Complex Float]

sixthroot :: Complex Float -> [Complex Float]
sixthroot x = pure x `bind` sqrt' `bind` cbrt'
-}

--
-- Random functions
--

type Rand a = StdGen -> (a, StdGen)

{-
bind :: Rand a -> (a -> Rand b) -> Rand b

pure :: a -> Rand a

lift :: (a -> b) -> a -> Rand b
-}

--
-- Monad instances
--

instance Monad Maybe where
    return x = Just x

    Nothing  >>= _ = Nothing
    (Just x) >>= f = f x

newtype Debuggable a = D (a, String)

newtype Multivalued a = M { unM :: [a] }

newtype Randomized a = R { unR :: StdGen -> (a, StdGen) }

instance Functor Debuggable where
    fmap f (D (x, s)) = D (f x, s)

instance Monad Debuggable where
    return x = D (x, "")

    D (x, s1) >>= f = let D (y, s2) = f x
                      in
                        D (y, s1 ++ s2)

instance Functor Multivalued where
    fmap f (M xs) = M (fmap f xs)

instance Monad Multivalued where
    return x = M [x]

    M xs >>= f = M $ concat (map unM (map f xs))

instance Functor Randomized where
    fmap f (R g) = R $ \s ->
        let (x, s') = g s
        in
            (f x, s')

instance Monad Randomized where
    return x = R $ \s -> (x, s)

    R x >>= f = R $ \s ->
                let (y, s') = x s
                in
                  unR (f y) s'

instance Applicative Maybe where
  pure  = return
  (<*>) = ap

instance Applicative Debuggable where
  pure  = return
  (<*>) = ap

instance Applicative Randomized where
  pure  = return
  (<*>) = ap

instance Applicative Multivalued where
  pure  = return
  (<*>) = ap
