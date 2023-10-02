-- Using a list comprehension, give an expression that calculates
-- the sum 1^2 2^2 ` . . . 100^2 of the first one hundred integer squares.
firsthundred :: Int
firsthundred = sum [x ^ 2 | x <- [1 .. 100]]

-- Suppose that a coordinate grid of size m ˆ n is given by the list of all pairs
-- px, yq of integers such that 0 ď x ď m and 0 ď y ď n. Using a list comprehension,
-- define a function grid :: Int -> Int -> [(Int,Int)] that returns a
-- coordinate grid of a given size. For example:

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- Using a list comprehension and the function grid above, define a function
-- square :: Int -> [(Int,Int)] that returns a coordinate square of size n,
-- excluding the diagonal from p0, 0q to pn, nq. For example:

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- square n = [(x, y) | x <- [0 .. n], y <- [0 .. n], x /= y]

-- In a similar way to the function length, show how the library function
-- replicate :: Int -> a -> [a] that produces a list of identical elements
-- can be defined using a list comprehension. For example:
-- > replicate 3 True
-- [True,True,True]

replicate :: Int -> a -> [a]
replicate x b = [b | _ <- [0 .. x]]

-- A triple px, y, zq of positive integers is Pythagorean if it satisfies the equation
-- x2 ` y2 “ z 2. Using a list comprehension with three generators, define a
-- function pyths :: Int -> [(Int,Int,Int)] that returns the list of all such
-- triples whose components are at most a given limit. For example:
-- > pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

-- We begin at 1 instead of 0 since we want positive integers
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

-- nestedcomprehension :: Int -> [(Int, Int)]
-- nestedcomprehension x = [(x,y) | y <- [3,4]]

-- comprehension :: [(Int, Int)]
-- comprehension = concat [nestedcomprehension x | y <- [3, 4]]

-- ncomp = [(x,y) | x <- [1,2], y <- [3,4]]







-- 1. Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
-- using the higher-order functions map and filter.
som = map (filter p xs)

-- 2. Without looking at the definitions from the standard prelude, define the following
-- higher-order library functions on lists.
-- a. Decide if all elements of a list satisfy a predicate:
-- all :: (a -> Bool) -> [Bool] -> Bool
-- b. Decide if any element of a list satisfies a predicate:
-- any :: (a -> Bool) -> [Bool] -> Bool
-- c. Select elements from a list while they satisfy a predicate:
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- d. Remove elements from a list while they satisfy a predicate:
-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- Note: in the prelude the first two of these functions are generic functions
-- rather than being specific to the type of lists.


-- 3. Redefine the functions map f and filter p using foldr.