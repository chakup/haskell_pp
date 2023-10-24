-- | Part 1. Addition Expressions.

-- | We start by defining the expressions. Each expression is either
--    a value or an addition of two expressions:
data Expr = Val Int | Add Expr Expr

-- | Possible Variants:

{-
  data Expr =
       Val Int | Add Expr Expr | Mult Expr Epr | Sub Expr Expr | ...

  data UnOp  = UMinus | UAbs
  data BinOp = BAdd | BMul | BSub
  data Expr  = Val Int | BinOp Expr Expr | UnOp Expr
-}

-- | We might want Haskell to print nicely our expressions.
instance Show Expr where
  show (Val n) = show n
  show (Add x y) =
    case (x, y) of
      (Add _ _, Add _ _) -> "(" ++ show x ++ ") + (" ++ show y ++ ")"
      (Add _ _, _) -> "(" ++ show x ++ ") + " ++ show y
      (_, Add _ _) -> show x ++ " + (" ++ show y ++ ")"
      otherwise -> show x ++ " + " ++ show y

expr1 = Add (Add (Val 1) (Val 2)) (Val 3)

-- Haskell now prints it nicely as (1 + 2) + 3

-- | Exercise 1 (*).
-- To avoid writing in big letters `Add` and `Val` (and also
--    to be able to use infix notation for addition, define functions
--
--   pls :: Expr -> Expr -> Expr
--   val :: Int -> Expr
--
--   such that for example we have:
--
--   (val 1 `pls` val 2) `pls` val 3 = Add (Add (Val 1) (Val 2)) (Val 3)
pls x y = Add x y

val x = Val x

-- Previous example written simply as:
expr1' = (val 1 `pls` val 2) `pls` val 3

-- (1 + 2) + 3

-- another example:
expr2 =
  ( (val 1 `pls` val 0)
      `pls` (val 0 `pls` val 5)
      `pls` (val 0 `pls` (val 5 `pls` val 0))
  )
    `pls` (val 0 `pls` (val 1 `pls` val 0))

-- (((1 + 0) + (0 + 5)) + (0 + (5 + 0))) + (0 + (1 + 0))

-- | Exercise 2. (***)
-- Define a function
--
--  folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
--
-- such that 'folde f g e' traverse the expression 'e' and
--  - applies f to the content of the Val constructor
--  - applies g to the results of the computation obtainted
--    by traversing the Add constructor recursively
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g e = case e of
  Val x -> f x
  Add e1 e2 -> g (folde f g e1) (folde f g e2)

-- | Exercise 3. (**)
-- Using folde function, define the function eval :: Expr -> Int
--    that evaluates the expresion into an integer value.
--
--  E.g.
--       eval (Val 1) = 1
--       eval (Add (Val 1) (Val 2)) = 3
--       eval ((val 1 `pls` val 2) `pls` val 3) = 6
eval e = folde (\i -> i) (\x y -> x + y) e

e2 = eval expr2

-- 12

-- | Exercise 4. (**)
-- Using folde function, define the function size :: Expr -> Int
--    that counts number of Values in the expression.
--
--  E.g.
--       size (Val 1) = 1
--       size (Add (Val 1) (Val 2)) = 2
--       size expr2 = 10
size e = folde (\_ -> 1) (\r1 r2 -> r1 + r2) e

s2 = size expr2

-- 10

-- | Exercise 5. (**)
-- Using folde function, define the function zeroes :: Expr -> Int
--    that counts number of zero values in the expression.
--
--  E.g.
--       zeroes (Val 0) = 1
--       zeroes (Val 4) = 0
--       zeroes (Add (Add (Val 0) (Val 1)) (Val 2)) = 1
--       zeores expr2 = 6
zeroes e = undefined

z2 = zeroes expr2

-- 6

-- | Exercise 6. (***)
-- Using folde function, define the function norm :: Expr -> Int
-- simplifies the expression by removing all zeroes from the compound
-- expression. (Indeed, for any number x, x + 0 = 0 + x = x).
--
--  E.g.
--       norm (Val 0) = (Val 0)
--       norm (Val 4) = (Val 4)
--       norm (Add (Add (Val 0) (Val 1)) (Val 2)) = Add (Val 1) (Val 2)
--       norm expr2 = Add (Add (Add (Val 1) (Val 5)) (Val 5)) (Val 1)
norm :: Expr -> Expr
norm = undefined

n0 = norm (Add (Add (Val 0) (Val 0)) (Add (Val 0) (Val 0)))

-- (0 + 0) + (0 + 0) ---> simplifies to 0 = n0

-- | Observe that after simplification the expression expr2 contains only 4 values and no zeroes. :)
n2 = norm expr2

-- ((1 + 5) + 5) + 1

sn2 = size n2

-- 4

zn2 = zeroes n2

-- 0
