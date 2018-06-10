import Data.List

data NestedList a = Elem a | List [NestedList a]

--1 Find the last element of a list.
myLast = head . reverse

--2  Find the last but one element of a list.
myButLast = last . init

--3  Find the K'th element of a list. The first element in the list is number 1.
elementAt xs k = xs !! (k-1)

--4 Find the number of elements of a list.
myLength = sum . map (\x -> 1)

--5 Reverse a list.
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

--6 Find out whether a list is a palindrome. A palindrome can be read forward or backward; 
isPalindrome xs = xs == reverse xs


--7 Flatten a nested list structure.
flatten :: NestedList a -> [a]
flatten a = case a of 
    Elem a     -> [a]
    List (x:xs)   -> flatten x ++ flatten (List xs)

--8 Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress = map head . group

--10 Run-length encoding of a list.
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . group 

--11 Modified run-length encoding.

data Modified a = Multiple Int a | Single a
encodeModified :: Eq a => [a] -> [Modified a]
encodeModified = 
    let helper x = case x of 
              (1, a) -> Single a
              (n, a) -> Multiple n a
    in map helper . encode

--12 Decode a run-length encoded list.
decodeModified :: [Modified a] -> [a]
decodeModified xs = concatMap helper xs
    where helper m = case m of
                    Multiple m a -> take m . repeat $ a
                    Single   a   -> [a]

--14 Duplicate the elements of a list.
dupli xs = concatMap (\x -> take 2 . repeat $ x) $ xs

--15 Replicate the elements of a list a given number of times.

repli xs n = concatMap (\x -> take n . repeat $ x) $ xs

--16 Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = 
    let iter (x:xs) i 
                | (i `mod` n == 0) = iter xs (i+1)
                | otherwise        = x : iter xs (i+1)
        iter [] _ = []
    in iter xs 1
    
    




