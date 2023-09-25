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
removeFst _ [] = []
removeFst x (y : ys) -- Looping through ys
  | x == y = ys -- If x == y then return ys
  | otherwise = y : removeFst x ys -- Assigning y to be the next in the list

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = minInt xs

count :: Char -> String -> Int
count _ [] = 0
count x (y : xs)
  | x == y = 1 + count x xs
  | otherwise = count x xs

blowup :: String -> String
blowup (x : xs)
  | 