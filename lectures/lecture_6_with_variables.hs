import Data.List

-- | Part 2. Addition Expressions with variables and environment.

-- | We start again by defining the expressions.
--    Each expression is now
--    - either a value or
--    - an addition of two expressions
--    - or a variable:
data Expr
  = Val Int
  | Add Expr Expr
  | Var String

-- | Again, we want Haskell to print nicely our expressions:
instance Show Expr where
  show (Val n) = show n
  show (Var s) = s
  show (Add x y) =
    case (x, y) of
      (Add _ _, Add _ _) -> "(" ++ show x ++ ") + (" ++ show y ++ ")"
      (Add _ _, _) -> "(" ++ show x ++ ") + " ++ show y
      (_, Add _ _) -> show x ++ " + (" ++ show y ++ ")"
      otherwise -> show x ++ " + " ++ show y

-- | Example
expr3 =
  Add
    (Add (Add (Var "x") (Val 2)) (Add (Var "y") (Val 3)))
    (Add (Var "x") (Var "y"))

-- ((x + 2) + (y + 3)) + (x + y)

-- | Exercise 0. (*)
-- Again, to avoid writing in big letters `Add` and `Val` or `Var`
--   (and also to be able to use infix notation for addition,
--   define functions
--
--   pls :: Expr -> Expr -> Expr
--   val :: Int -> Expr
--   var :: String -> Expr
pls x y = Add x y

val i = Val i

var x = Var x

-- | Example
expr3' =
  ((var "x" `pls` val 2) `pls` (var "y" `pls` val 3))
    `pls` (var "x" `pls` var "y")

-- ((x + 2) + (y + 3)) + (x + y)

-- | Exercise 1. (***)
-- Define a function
--
--  foldex :: (Int -> a) -> (String -> a) -> (a -> a -> a) -> Expr -> a
--
-- such that 'folde f g h e' traverse the expression 'e' and
--  - applies f to the content of the Val constructor
--  - applies g to the variable in the Var constructor
--  - applies h to the results of the computation obtainted
--    by traversing the Add constructor recursively
foldex :: (Int -> a) -> (String -> a) -> (a -> a -> a) -> Expr -> a
foldex f g h e = case e of
  Val x -> f x
  Var k -> g k
  Add e1 e2 -> h (foldex f g h e1) (foldex f g h e2)

-- | Exercise 2. (**)
-- Define a function union_f :: (Eq a) => [a] -> [a] -> [a]
-- such that "union_f l1 l2" returns a list representing
-- the union of two sets "l1" "l2" (represented themselves as lists).
--
-- NB: make sure that if an element x is both in l1 and l2,
-- then x appears only once in the "union_f l1 l2".
--
-- Hint: use "foldl" or "foldr".
union_f :: (Eq a) => [a] -> [a] -> [a]
union_f l r = foldl (\res x -> if elem x res then res else x : res) l r

-- | Test:
ut = union_f ["x", "y"] ["y", "z", "x"]

-- ["z","x","y"]

-- | Exercise 3.  (**)
-- Using "foldex" function, define a function vars :: Expr -> [String]
-- that returns the set of variables of the given expression.
--
-- E.g.
--    vars (Var x) = ["x"]
--    vars (Add (Val 1) (Add (Var x) (Var y))) = ["x", "y"]
--    vars (Add (Add (Var 2) (Var 3)) (Val 5)) = []
vars e = foldex (\n -> []) (\x -> [x]) (\r1 r2 -> union_f r1 r2) e

-- | Test:
v3 = vars expr3

-- ["y","x"]

-- | Exercise 4. (*)
--   Define a function get :: String -> [(String, Int)] -> Int
--   that takes a variable (x :: String) and the environment
--   (env :: [(String, Int)]) and returns the value associated in the
--   environment to the variable x. If x is not in the environment,
--   it should call error MSG function, i.e.
--
--      error ("Variable '" ++ x ++
--                       "' not found in the environemnt " ++ show env)
--
--   Hint: use lookup function from lists lib.
get :: String -> [(String, Int)] -> Int
get k env = case lookup k env of
  Nothing -> error ("Variable '" ++ k ++ "' not found in the environemnt " ++ show env)

-- | Exercise 5. (**)
-- Define the evaluation function eval :: [(String, Int)] -> Expr -> Int
-- that given an environment "env" and an expression "e" returns
-- the evaluation of "e" where each occurence of each variable in the
-- expression is evaluated to the corresponding value in the environment.
-- Hint: again, use foldex.
eval :: [(String, Int)] -> Expr -> Int
eval env e = foldex (\i -> i) (\x -> get x env) (\a b -> a + b) e

-- | Tests:
e3fa = eval [] expr3

-- Exception: Variable 'x' not found in the environemnt []

e3fb = eval [("x", 2)] expr3

-- Exception: Variable 'y' not found in the environemnt [("x",2)]

e3 = eval [("x", 2), ("y", 3)] expr3

-- 15
-- Indeed, 15 = ((x[x=2] + 2) + (y[y=3] + 3)) + (x[x=2] + y[y=3])
--            = ((2 + 2) + (3 + 3)) + (2 + 3)
