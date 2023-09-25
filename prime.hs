-- A list of guarded equations such as
-- foo t | condition_1 = body_1
-- foo t | condition_2 = body_2
-- foo t | condition_3 = body_3
-- foo t = body_4

-- can be abbreviated as

-- foo t | condition_1 = body_1
--       | condition_2 = body_2
--       | condition_3 = body_3
--       | otherwise = body_4

-- in case condition_1 holds, foo t is by definition equal to body_1,
-- in case condition_1 does not hold but condition_2 holds, foo t is by definition equal to body_2,
-- in case condition_1 and condition_2 do not hold but condition_3 holds, foo t is by definition equal to body_3,
-- and in case none of condition_1, condition_2 and condition_3 hold, foo t is by definition equal to body_4.
-- divides :: Integer -> Integer -> Bool -- type declaration, divides :: (argument type)-> (produces result(takes argument integer) and returns boolean)
divides d n = rem n d == 0

ldf k n
  | divides k n = k
  | k ^ 2 > n = n
  | otherwise = ldf (k + 1) n

ld :: Integral t => t -> t
ld n = ldf 2 n

prime0 n
  | n < 1 = error "not a positive integer"
  | n == 1 = False
  | otherwise = ld n == n