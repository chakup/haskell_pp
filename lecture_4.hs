finddif :: Eq a => a -> [(a,b)] -> [b]
finddif k t = [v | (k', v) <- t, k == k']

--finddif 'b' [('a',1),('b',2),('c',3),('b',4)]
-- 5.7.1, 5.7.2, 5.7.3, 5.7.7, 5.7.9 




