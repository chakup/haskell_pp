-- Graham Huttons lecture
-- In mathematics the comprehension notation can be used used to const

--- 7.9.1, 7.9.2.a, 7.9.2.b, 7.9.2c, 7.9.2.d, 7.9.5

-- What are higher-order functions that return functions as results better known as?
-- take :: Int -> [a] -> [a]
-- Curried functions

-- map f (filter p xs)

-- 7.9.1 Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
-- using the higher-order functions map and filter

-- map f (filter p xs)

-- 7.9.2 Without looking at the definitions from the standard prelude, define the fol-
-- lowing higher-order library functions on lists.

-- a. Decide if all elements of a list satisfy a predicate

all :: (a -> Bool) -> [Bool] -> Bool
all f (x : xs) = foldr f x

-- exa = all even [1,2,3,4]

-- ex_a = alls (&&) [True, False, False]
-- b. Decide if any element of a list satisfies a predicate
-- any :: (a -> Bool) -> [Bool] -> Bool
-- c. Select elements from a list while they satisfy a predicate
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- d. remove elements from a list while they satisfy a predicate
-- dropWhile :: (a -> Bool) -> [a] -> [a]

-- Note: in the prelude the first two of these functions are generic functions
-- rather than being specific to the type of lists