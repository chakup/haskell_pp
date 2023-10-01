-- 3.11.1. What are the types of the following values?
-- [’a’,’b’,’c’] A list of characters
-- (’a’,’b’,’c’)[(False,’O’),(True,’1’)] Tuple
-- ([False,True],[’0’,’1’]) A tuple of list
-- [tail, init, reverse] A list of functions

-- 3.11.2. Write down definitions that have the following types; it does not matter what the definitions actually do as long as they are type correct.
bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[9]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy a = (a, a)

-- apply :: (a -> b) -> a -> b
-- apply t a =

-- 3.11.3. What are the types of the following functions?

--     second xs = head (tail xs)
--     swap (x,y) = (y,x)
--     pair x y = (x,y)
--      double :: Int -> Int
--     double x = x*2
--      palindrome :: [] -> []
--     palindrome xs = reverse xs == xs
--     twice f x = f (f x)
-- Hint: take care to include the necessary class constraints in the types if the functions are defined using overloaded operators.

sumdown :: Int -> Int
sumdown x
  | x <= 0 = 0
sumdown x = x + sumdown (x - 1)

sq :: Int -> Int -> Int
sq _ 0 = 1
sq a b = a * (sq a (b - 1))

-- ( ^ ) :: Int -> Int -> Int
-- _ ^ 0 = 1
-- a ^ b = a * (a ^ (b - 1))

-- 2 ^ 3
-- = 2 * (2 ^ 2) -- Applying the recursive case
-- = 2 * (2 * (2 ^ 1)) -- Applying the recursive case again
-- = 2 * (2 * (2 * (2 ^ 0))) -- Applying the recursive case one more time
-- = 2 * (2 * (2 * 1)) -- Applying the base case
-- = 2 * (2 * 2) -- Simplifying
-- = 2 * 4 -- Simplifying
-- = 8 -- Final result