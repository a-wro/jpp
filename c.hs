reduce :: (a -> b -> b) -> b -> [a] -> b

reduce _ x []    = x
reduce f x (h:t) = f h (reduce f x t)

--HS.C1(6)
maxNat = reduce max 0 

--HS.C2(8)
count f = reduce (\x acc -> if f x then acc + 1 else acc) 0 

--HS.C3(8)
rev = reduce (\x acc -> acc ++ [x]) []

--HS.C4(10)
maxNum lst = case maxNat lst of
    0 -> Nothing
    x -> Just x
