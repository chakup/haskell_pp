{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | -------------------------------------------------------------------------

-- | -----------------------  PART 5. EXERCISES  -----------------------------

-- | -------------------------------------------------------------------------

-- | -------------------------------------------------------------------------

-- | Exercise 1: Selector.
--    Implement a function 'selector f g b' such that if the condition 'b' is true,
--    it applies the function 'f' and otherwise applies function 'g'.
selector :: (t1 -> t2) -> (t1 -> t2) -> Bool -> t1 -> t2
selector f g b = if b then f else g

fpm n = selector (\x -> x + 1) (\x -> x - 1) (even n) n

t54 = fpm 55

-- 54

-- | -------------------------------------------------------------------------

-- | Exercise 2: altMap.
-- Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- that alternately applies its two argument functions to successive
-- elements in a list, in turn about order. For example:
--
--  > altMap (+10) (+100) [0,1,2,3,4]
--    [10,101,12,103,14]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

-- |                  (altMap f g [x,y,z]) =
--   f x :             (altMap g f   [y,z]) =
--   f x : g y :       (altMap f g     [z]) =
--   f x : g y : f z : (altMap g f      []) =
--   [f x, g y, f z]
tam = altMap (+ 10) (+ 100) [0, 1, 2, 3, 4]

--    [10,101,12,103,14]

-- | -------------------------------------------------------------------------

-- | Exercise 3: fold_nat.
--    Observe the following definitions recursing on the integer argument 'n'.
--    Define a generic 'fold_nat' function in the same spirit that 'foldr' is defined on lists
--    and redefine the functions below using 'fold_nat'. Start by writing the type of 'fold_nat.
fold_nat :: (a -> b) -> a -> a
fold_nat f n = 


fact 0 = 1
fact n = n * (fact (n - 1))

init_list c 0 = []
init_list c n = c : (init_list c (n - 1))

initl_n 0 = []
initl_n n = n : (initl_n (n - 1))

apply_f_n :: (t -> t) -> Int -> t -> t
apply_f_n f n x
  | n <= 0 = x
  | otherwise = apply_f_n f (n - 1) (f x)

til = init_list 1 10

-- [1,1,1,1,1,1,1,1,1,1]
t5to1 = initl_n 5

-- [5,4,3,2,1]
f5 = fact 5

-- 120
p64 = apply_f_n (2 *) 6 1

-- 64

init_listf c n = undefined

initl_nf n = undefined

fact_f n = undefined

apply_f_nf f n x = undefined

tilf = init_listf 1 10

-- [1,1,1,1,1,1,1,1,1,1]
t5to1f = initl_nf 5

-- [5,4,3,2,1]
f5' = fact_f 5

-- 120
p64' = apply_f_nf (2 *) 6 1

-- 64

-- | -------------------------------------------------------------------------

-- | Exercise 4: Foldl.
--    We also have the following fold function called foldl:
--
--      foldl :: (a -> b -> a) -> a -> [b] -> a
--      foldl f v [] = v
--      foldl f v (x:xs) = foldl f (f v x) xs
--
--      Question 1. What are the differences between foldl and foldr?
--      Question 2. Define reverse using foldl.
--      Question 3. Define reverse using foldr.

-- | -------------------------------------------------------------------------

-- | Exercise 5.
--    Define a function that applies a function f n-times
--    to the 'length-n'-th element of the list, i.e. for a given list of length
--    'n' it applies 'f' n-times to the first element, applies 'f' 'n-1' times
--    to the second element, and so on.
--
-- [    x_1,       x_2,       ...,        x_(n-1),     x_n ]
--       _          _          _             _           _
--       |          |          |             |           |
--       v          v          v             v           v
-- [ f^n x_1,    f^(n-2),     ...,      f^1 x_(n-1),    x_n ]
iter_f' :: (t -> t) -> [t] -> [t]

-- | Hint 1: you can use the apply_f_n function.
iter_f' f l = undefined

-- | Question 5.1: Define iter_f without using apply_f_n ?
iter_f :: (t -> t) -> [t] -> [t]
iter_f f l = undefined

-- | Question 5.2: Define iter_f without using apply_f_n but using foldr.
iter_fr :: (t -> t) -> [t] -> [t]
iter_fr f l = undefined

-- | -------------------------------------------------------------------------

-- | Exercise 6.1. Define a function power_2 l that transforms a list
--    representing a binary number into a list of powers of 2, e.g.
--
--      powers_2   [1,1,1,1,1] =    [16,8,4,2,1]
--      powers_2 [1,1,0,1,1,0] = [32,16,0,4,2,0]

-- | We can now interpret convert number into a decimal by first defining:
powers_2 l = undefined

vl31 = powers_2 [1, 1, 1, 1, 1]

-- [16,8,4,2,1]
vl22 = powers_2 [1, 0, 1, 1, 0]

-- [16,0,4,2,0]

-- | Exercise 6.2. Define a function bin2dec' that converts a list representing a binary
--    number into a integer. e.g.
--
--    bin2dec' [1,1,1,1,1] = 31
--    bin2dec'   [1,0,1,1] = 13
bin2dec' :: [Int] -> Int
bin2dec' = undefined

vb31' = bin2dec' [1, 1, 1, 1, 1]

-- 31
vb13' = bin2dec' [1, 0, 1, 1]

-- 13

-- | Exercise 6.3. define bin2dec directly by using 'foldr' :
bin2dec :: [Int] -> Int
bin2dec l = undefined

vb4 = bin2dec [0, 0, 1, 0, 0]

-- 4
vb29 = bin2dec [1, 0, 1, 1, 1]

-- 29
