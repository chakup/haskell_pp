minInt :: [Int] -> Int
minInt [] = error "empty list"
minInt [x] = x
minInt (x : xs) = min x (minInt xs)

-- (x : xs) refers to x in a list of xs and xs is a list

min' :: Int -> Int -> Int
min' x y
  | x <= y = x
  | otherwise = y

maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [z] = z
maxInt (z : zs) = max z (maxInt zs) -- Give x and give the remainder of list

removeFst :: Int -> [Int] -> [Int]
removeFst [] = []
removeFst ls = drop 1 ls

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = minInt xs
