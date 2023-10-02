import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c+n) 'mod' 26)
    | otherwise = c04


encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


-- 5.7.1, 5.7.2, 5.7.3, 5.7.7, 5.7.9 

