{-# OPTIONS_GHC -Wall #-}

--
-- Binary Numerals
--
module While where

data N = Zero
       | One
       | NumThenZero N
       | NumThenOne N
  deriving(Eq, Ord, Show, Read)

numOf :: N -> Integer
numOf Zero            = 0
numOf One             = 1
numOf (NumThenZero n) = 2 * numOf n
numOf (NumThenOne n)  = 2 * numOf n + 1

--
-- While language
--

type Var = String

data Aexp = Const Integer | Var Var
          | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp
  deriving (Show)

data Bexp = BTrue | BFalse
          | Eq Aexp Aexp | Le Aexp Aexp
          | Not Bexp | And Bexp Bexp
  deriving (Show)

data Stm  = Assign Var Aexp | Skip | Seq Stm Stm
          | If Bexp Stm Stm
          | While Bexp Stm
  deriving (Show)

type Z = Integer

type State = Var -> Z

--
-- Semantics of an Arithmetic Expression
--

aSem :: Aexp -> (State -> Z)
aSem _ = error "aSem: unimplemented"

--
-- Semantics for Boolean Expressions
--

bSem :: Bexp -> (State -> Bool)
bSem _ = error "bSem: unimplemented"
