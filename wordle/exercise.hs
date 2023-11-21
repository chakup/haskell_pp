import System.IO (hSetEcho)
game = "beach"

wordle :: IO()
wordle = do putStrLn "_ _ _ _ _"
            word <- sgetLine
            putStrLn "Try to guess it"
            play word

play :: String -> IO()
play word = do  putStr "?"
                guess <- getLine
                if  guess == word then
                    putStrLn "You got it!!"
                else
                    do putStrLn(match word guess)
                       play word



match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

getCh :: IO Char
getCh = do hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
                else
                    do putChar '-'
                       xs <- sgetLine
                       return (x:xs)


adder :: IO()
adder = do putStr "How many numbers?:"
            n <- getLine
            nums <- getnums (read n :: Int)
            putStrLn("Their total is: " ++ show (sum nums))
            