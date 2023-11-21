import Data.Char

--------------------------------------------------------------------------------
-- Exercise 1. Implement functors for the following datatypes:

-- Exercise 1.a
data Option a
  = None
  | Some a
  deriving (Show)

instance Functor Option where
  fmap :: (a -> b) -> Option a -> Option b
  fmap g (Some a) = Some (g a)
  fmap _ None = None

--  fmap g ov = do {x <- ov; g <$> x} -- We can only use do for monads

-- Exercise 1.b
data BinarySearchTree a
  = Branch (BinarySearchTree a) a (BinarySearchTree a)
  | Leaf

instance Functor BinarySearchTree where
  fmap :: (a -> b) -> BinarySearchTree a -> BinarySearchTree b
  fmap f (Branch m n l) = Branch (f m) () ()
  fmap _ Leaf = Leaf

-- Exercise 1.c
data NonEmpty' a = NonEmpty'
  { neHead :: a,
    neTail :: [a]
  }
  deriving (Show)

instance Functor NonEmpty' where
  fmap :: (a -> b) -> NonEmpty' a -> NonEmpty' b
  fmap f (NonEmpty' x xs) = undefined

--------------------------------------------------------------------------------
-- Exercise 2. Implement the following functions without using fmap.

-- Exercise 2.1.a. Implement `try_find_list` below that
-- returns the first element of the list that satisfies the predicate p.
try_find_list :: (a -> Bool) -> [a] -> Option a
try_find_list p l = undefined

-- Exercise 2.1.b. Implement `try_transform_list p g l` below that
-- applies the function `g` to the element in the list `l` satisfying
-- the predicate `p`, if any such element exists, and otherwise
-- returning None.
try_transform_list p g l = undefined

-- Exercise 2.2.a. Implement `try_find_tree` below that
-- returns the first element of the BST that satisfies the predicate p,
-- searching in the infix order and then from left to right.
try_find_tree p t = undefined

-- Exercise 2.2.b. Implement `try_transform_tree p g t` below that
-- applies the function `g` to the element in the tree `t` satisfying
-- the predicate `p`, if any such element exists, and otherwise
-- returning None.
try_transform_tree p g t = undefined

-- | Exercise 2.3. Now implement functions
--    'try_transform_list' and 'try_transform_tree' using fmap.
--    NB: give them a different name to avoid compiler complaining.
--    Question: fmap of which Functor shall you use?
try_transform_list' p g l = undefined

try_transform_tree' p g t = undefined

--------------------------------------------------------------------------------

-- | Exercise 3. Implement a function `toUpStr` that converts strings to upper
--   case (toUpStr :: String -> String) without using toUpper.
--   Expected behaviour:
--     toUpStr "" = ""
--     toUpStr "abc" = "ABC"
--
--   Hints:
--     - use the fact that string is a list of char, so you can also use fmap
--     - use the following functions:
--       -- ord :: Char -> Int
--       -- chr :: Int -> Char, e.g. chr 100 = 'd' and chr 68 = 'D'
--       -- subtract :: Num a => a -> a -> a, e.g. subtract subtract 34 3 = - 31
caseDiff = ord 'a' - ord 'A'

-- 32

-- Exercise 3.1. Write the definition applying several `fmap`s sequentially (using (.))
toUpStr str = undefined

-- Exercise 3.2. Use fmap only once! Hint: think about function composition on char type.
toUpStr' str = undefined

--------------------------------------------------------------------------------
-- Exercise 4. Implement the functor for the following type applied to
-- a given type variable, i.e. for the type 'FunF a'.
-- (Why can't you construct a functor directly for 'FunF'?)
-- What can you observe about fmap for the type 'FunF a'?

data FunF a b = FF (a -> b)

-- instance Functor (FunF a) where
--   ...
