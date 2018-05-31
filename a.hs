--HS.A1(1)

square x = x*x

--HS.A2(1)
first a _ = a

--HS.A2
listAdd :: Num a => [a] -> a
listAdd = sum

listAdd2 :: Num a => [a] -> a
listAdd2 [] = 0
listAdd2 (x:xs) = x + listAdd2 xs

--HS.A4(2)
listMember :: Eq a => [a] -> a -> Bool
listMember = flip elem

--foldl1 (||) == or
listMember2 xs a = or . map (\x -> if x == a then True else False) $ xs

--HS.A5(2)
listAppend = (++)

--listAppend2 [] ys = ys
--listAppend2 xs [] = xs
--listAppend2 (x:xs) ys = x:ys ++ listAppend2 xs

--HS.A6(3)
listMax [] = 0
listMax xs = foldl1 max xs

--HS.A6(6)
listMax2 :: Ord a => [a] -> Maybe a
listMax2 [] = Nothing
listMax2 xs = Just (foldl1 max xs)

--HS.A7(3)
listRev = reverse

listRev2 :: [a] -> [a]
listRev2 [] = []
listRev2 [x] = [x]
listRev2 (x:xs) = listRev2 xs ++ [x]