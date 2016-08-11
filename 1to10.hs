import Data.List

-- 1: Find the last element of a list
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [e] = Just e
myLast (e:es) = myLast es

-- 2: Find the last but one element of a list
myButLast :: [a] -> a
myButLast = last . init

-- 3: Find the Kth element in a list. The list is 1 based
elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:_) 1 = Just x
elementAt (_:xs) n = elementAt xs (n - 1)

-- 4: Find the number of elements in a list
myLength :: [a] -> Integer
myLength = foldl (\n _ -> n + 1) 0


-- 5: Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 6: Palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome chars = chars == (reverse chars)

--7: Flatten a list
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (el:xs)) = flatten el ++ flatten (List xs)

--8: Eliminate consecutive duplicates of list elements
-- My solution:
-- compress :: (Eq a) => [a] -> [a]
-- compress [] = []
-- compress [a] = [a]
-- compress (x:y:xs) = if x == y then compress (y:xs) else x:compress (y:xs)
-- Best solution from Haskell.org:
compress :: Eq a => [a] -> [a]
compress = map head . group

--9: Pack consecutive duplicates of list elements into sublists
-- pack :: Eq a => [a] -> [[a]]
-- pack = group
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:first) : pack rest
  where
    getReps [] = ([], [])
    getReps (y:ys)
      | y == x = let (f,r) = getReps ys in (y:f, r)
      | otherwise = ([], (y:ys))
    (first, rest) = getReps xs

-- 10: Run-length encoding of list
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

-- 11: Modify run-length such that where an element has no duplicates
-- it is tagged as Single, and if there are multiples it is tagged as Multiple

data Encoded a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map encodeHelper . encode
  where
    encodeHelper (1,x) = Single x
    encodeHelper (n,x) = Multiple n x

-- 12: Decode a run-length encoded list
decodeModified :: Eq a => [Encoded a] -> [a]
-- decodeModified [] = []
-- decodeModified (Single a:as) = [a] ++ decodeModified as
-- decodeModified ((Multiple n a):as) = (take n . repeat $ a) ++ decodeModified as 
decodeModified = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x


-- 13: Run-length encoding of a list (direct solution)
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect = foldr directHelper []
  where 
        directHelper x [] = [Single x]
        directHelper x ((Single y):ys)
          | x == y = (Multiple 2 y):ys
          | otherwise = Single x : (Single y):ys
        directHelper x ((Multiple n y):ys)
          | x == y = let t = n + 1 in (Multiple t y):ys
          | otherwise = Single x : ((Multiple n y):ys)


--14 Duplicate the elements of a list
dupli :: [a] -> [a]
-- dupli = foldr dupliHelper []
--   where
--     dupliHelper x acc = x:x:acc
dupli [] = []
dupli (x:xs) = x:x:dupli xs


--15 Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ repli xs n
