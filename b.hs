import Data.List (sortBy)
import Data.Ord (comparing)

--HS.B1(3)
myMapSquare = map (^2)

--HS.B2(4)
count f  = length . filter f
count2 f = sum . map (\x -> if f x then 1 else 0) 

--SCH.B3(4)
grep = filter

grep2 f = concatMap (\x -> if f x then [x] else [])

grep3 f (x:xs) 
    | f x = x : grep3 f xs
    | otherwise = grep3 f xs
grep3 _ [] = []


--SCH.B4(4)
ordered :: (a -> a -> Bool) -> [a] -> Bool
ordered op xs = and . zipWith op xs $ tail xs


--SCH.B5(6)
mySort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]  
mySort _ [] = []  
mySort op (x:xs) =   
    let left = mySort op [ a | a <- xs, a `op` x ]  
        right = mySort op [ a | a <- xs, not $ a `op` x ]  
    in  left ++ [x] ++ right  
