-- | -------------------------------------------------------------------------
--   Lecture 2. Part 1. Sorting Algorithms.
--   Based on the materials from J.C.Filliâtre Compilers lecture
--    https://www.lri.fr/~filliatr/ens/compil/
--   -------------------------------------------------------------------------

-- | Replace TODOS by the actual implementation
_TODO0_ = []

_TODO1_ = ([], [])

-- | -------------------------------------------------------------------------

-- | Exercise 1.1 Insertion Sort

-- | -------------------------------------------------------------------------

-- |  Implement insert_elem function which inserts an element into a
--     list of integers supposedly sorted in ascending order.

-- A method

insert_elem :: Ord a => a -> [a] -> [a]
insert_elem x [] = [x]
insert_elem x (y : l) =
  if x < y
    then x : y : l
    else y : (insert_elem x l)

-- insert_elem' :: Ord t => t -> [t] -> [t]
-- insert_elem' x [] = [x]
-- insert_elem' x (y : l)
--   | x < y = x : y : l
--   | otherwise = y : (insert_elem' x l)

-- |  Write an insertionSort function which performs insertion sorting of a list
--     of integers. Remember that this consists of extracting the first element
--     from the list, recursively sorting the rest of the list, then
--     inserting the isolated element (with the insert function).
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x : xs) = insert_elem x (insertionSort xs)

test1 = insertionSort [21, 1, 13, 5, 8, 1, 2, 3]

-- | -------------------------------------------------------------------------

-- | Exercise 1.2. Merge Sort

-- | -------------------------------------------------------------------------

-- | Write a 'split' which takes a list 'l' as an argument and “cuts it in two”,
--    i.e. returns a pair of lists (l1,l2) such that the
--    elements of l are exactly those of l1 and l2 and such that the lists l1 and
--    l2 have a length not differing at most by one.
split :: Ord a => [a] -> ([a], [a])
split [] = ([], []) -- If the list is empty then
split (x : xs) = (x : l2, l1) -- x : xs,,,, så
  where
    (l1, l2) = split xs -- xs is the arg

split' xs = (firstHalf xs, secondHalf xs)
  where
    firstHalf xs = let n = length xs in take (div n 2) xs
    secondHalf xs = let n = length xs in drop (div n 2) xs

-- Should be equal to ([2,5,7,6,9],[1,8,3,0,4])
test11 = split [2, 1, 5, 8, 7, 3, 6, 0, 9, 4]

-- | Write a 'fusion' function which takes as arguments two lists supposedly
--    sorted in ascending order and merges them into a single sorted list.
fusion :: Ord a => [a] -> [a] -> [a]
fusion [] [] = []
fusion l1 [] = l1
fusion [] l2 = l2
fusion (x1 : l1) (x2 : l2)
  | x1 <= x2 = x1 : (fusion l1 (x2 : l2))
  | otherwise = x2 : (fusion (x1 : l1) l2)

-- Should be equal to [1,2,3,5,6,7,9]
test2 = fusion [1, 6, 7] [2, 3, 5, 9]

-- | Using the two previous functions, write a 'mergeSort' function performing
--    the merge sort. Remember that the principle consists of cutting the list
--    in two, recursively sorting each of the two lists obtained, and then
--    merging the two sorted lists.
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = fusion (mergeSort l1) (mergeSort l2)
  where
    (l1, l2) = split l

test3 = mergeSort [21, 1, 13, 5, 8, 1, 2, 3]

-- | -------------------------------------------------------------------------

-- | Exercise 1.3 Heap Sort

-- | -------------------------------------------------------------------------

-- | A heap is a binary tree containing integers at each of its nodes and each
--    subtree t satisfies the following property: the integer at the root of t is
--    less than or equal to all the integers contained in t. Here is an example
--    of a heap containing the integers 1, 2, 2, 4, 5 and 6:
--
--           1
--          / \
--         2   5
-- 	/ \
--       4   2
--            \
-- 	     6
-- The goal of this question is to perform the two elementary operations on
-- the heap structure which are the insertion of a new element and the extraction
-- of the smallest element (i.e. the root).
-- We start by defining the following heap type to represent the heaps:
data Heap = Null | Fork (Int, Heap, Heap)

empty_heap = Null

h1 =
  Fork
    ( 1,
      Fork
        ( 2,
          Fork (4, Null, Null),
          Fork
            ( 2,
              Null,
              Fork (6, Null, Null)
            )
        ),
      Fork (5, Null, Null)
    )

string_of_heap Null = "Null"
string_of_heap (Fork (x, l, r)) =
  "(" ++ show x ++ ", " ++ (string_of_heap l) ++ ", " ++ (string_of_heap r) ++ ")"

e1 = string_of_heap h1

-- | "(1, (2, (4, Null, Null), (2, Null, (6, Null, Null))), (5, Null, Null))"

-- | Replace TODOS by the actual implementation
_TODO2_ = Null

_TODO3_ = (Nothing, Null)

-- fusion :: Ord a => [a] -> [a] -> [a]
-- fusion [] [] = []
-- fusion l1 [] = l1
-- fusion [] l2 = l2
-- fusion (x1 : l1) (x2 : l2)
--   | x1 <= x2 = x1 : (fusion l1 (x2 : l2))
--   | otherwise = x2 : (fusion (x1 : l1) l2)

-- | We start with a complex auxiliary operation, 'merge' which merges two heaps
--    a and b. The principle is as follows:
--     - If a or b is Null, the result is immediate.
--     - Otherwise, we compare the root of a and that of b. Suppose that the root
--       of a is the smallest (otherwise, we exchange the roles of a and b so the
--       implementation is symmetrical). The result is then a heap whose root is
--       that of a, whose left subtree is the right subtree of a, and whose right
--       subtree is obtained by recursively merging the left subtree of a and
--       tree b.  Write the merge function.
merge_heaps :: Heap -> Heap -> Heap
merge_heaps Null h2 = h2
merge_heaps h1 Null = h1
merge_heaps (Fork (x, l1, r1)) (Fork (y, l2, r2))
    | x <= y =
        let res = merge_heaps l1 (Fork (y, l2, r2)) in
        Fork (x, r1, res)
    | otherwise
        let res = merge_heaps l2 (Fork (x, l1, r1)) in
        Fork (y, l2, res)

-- | Using the merge function, write an 'add' function that inserts a new
--    element into a heap.  Hint: we can use the Fork heap (x, Null, Null)
--    which only contains x.
add :: Int -> Heap -> Heap
add x h = merge_heaps(Fork(x, Null, Null)) h

-- | Similarly, use the merge function to write an 'extract_min' function which
--    extracts the smallest element from a heap (its root) and returns the heap
--    made up of all the other elements. Hint: to deal with the case when the
--    heap is empty, we can use the value Nothing of the type
--    Maybe a = Nothing | Just a
extract_min :: Heap -> (Maybe Int, Heap)
extract_min Null = (Nothing, Null)
extract_min (Fork (r, sl, sr)) = (Just r, merge_heaps sl sr)

-- Should give 1
m1 =
  case extract_min h1 of
    (Nothing, _) -> -1
    (Just n, _) -> n

-- | Once the heap structure is given, it is easy to deduce a sorting algorithm,
--    called heap sort. To do this, start by writing a 'heap_of_list' function
--    which constructs a heap with all the elements of a list.
heap_of_list :: [Int] -> Heap
heap_of_list [] = empty_heap
heap_of_list (x:xs) = add x (heap_of_list xs) 

-- Should give
-- "(1, (1, Null, (2, (3, Null, Null), (5, Null, Null))), (8, (13, Null, Null), (21, Null, Null)))"
h2 = heap_of_list [21, 1, 13, 5, 8, 1, 2, 3]

e2 = string_of_heap h2

-- |
--            1
--          /   \
--         /     \
--        1       8
--         \     / \
--          2   13 21
--         / \
--        3   5

-- | Then write the inverse function 'list_of_heap' which constructs a sorted
--    list with all the elements of a heap.
list_of_heap :: Heap -> [Int]
list_of_heap h = _TODO0_

-- | Finally, combine the two previous functions into a 'heapsort' function
--    performing the heap sort.
heapSort :: [Int] -> [Int]
heapSort l = _TODO0_

test4 = heapSort [21, 1, 13, 5, 8, 1, 2, 3]

-- | -------------------------------------------------------------------------

-- | Main function for testing

-- | -------------------------------------------------------------------------

-- | Should print.

-- |
--   [1,1,2,3,5,8,13,21]
--   True
--   [1,2,3,5,6,7,9]
--   True
--   [1,1,2,3,5,8,13,21]
--   True
--   [1,1,2,3,5,8,13,21]
--   True
main = do
  print test1
  print (test1 == [1, 1, 2, 3, 5, 8, 13, 21])
  print test2
  print (test2 == [1, 2, 3, 5, 6, 7, 9])
  print test3
  print (test3 == [1, 1, 2, 3, 5, 8, 13, 21])
  print test4
  print (test4 == [1, 1, 2, 3, 5, 8, 13, 21])
