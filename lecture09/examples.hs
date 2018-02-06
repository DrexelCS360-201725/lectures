module Test where

import Prelude hiding (map, filter, take, drop)

--
-- Functions in Haskell
--
fact :: Integer -> Integer
fact n = if n == 0
         then 1
         else n * fact (n-1)

fact' :: Integer -> Integer
fact' 0 = 1
fact' n = n * fact' (n-1)

fact'' :: Integer -> Integer
fact'' n | n == 0    = 1
         | otherwise = n * fact (n-1)

-- Function with multiple arguments

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

ex16 :: Int -> Int -> Int
ex16 = add3 3

-- Try this:
--add x = x + "String"

--
-- List operations
--

-- map ::
map = undefined

-- append ::
append  = undefined

--
-- Define a safe head function
--

data MaybeHead = NoHead
               | JustHead Int

safeHeadInts = undefined

safeHead = undefined

--
-- More list operations
--

-- take ::
take = undefined

-- drop ::
drop = undefined


--
-- define a List data type
--

-- data List

--
-- define a NonEmptyList data type and associated functions
--

-- data NonEmptyList

-- nelToList ::

-- listToNel ::

-- headNEL ::

-- tailNEL ::

--
-- Higher-order functions
--

-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- ($) :: (a -> b) -> a -> b

-- What about papply?

--
-- Write an Eq instance for Foo
--

data Foo = F Int | G Char

-- instance Eq Foo where

