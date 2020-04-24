-- Find min1imum and max1imum from given list
-- load into ghci as :l minMax.hs

l = [1, 2, 3, 4, 5, 6, 7];

-- From Data.Foldable
-- λ> :t maximum
-- maximum :: (Foldable t, Ord a) => t a -> a
-- λ> :t minimum
-- minimum :: (Foldable t, Ord a) => t a -> a

-- Version 1

min1 [] = 0
min1 (x:xs) = min1' x xs
min1' n [] = n
min1' n (x:xs) = if x < n then min1' x xs else min1' n xs

max1 [] = 0
max1 (x:xs) = max1' x xs
max1' n [] = n
max1' n (x:xs) = if x > n then max1' x xs else max1' n xs

-- Version 2 Using Builtin min max functions

min2 [] = 0
min2 [x] = x
min2 [x, y] = min x y
min2 (x:y:xs) = min2 $ (min x y) : xs

max2 [] = 0
max2 [x] = x
max2 [x, y] = max x y
max2 (x:y:xs) = max2 $ (max x y) : xs

-- Version 3 Using Maybe and explicit type signature

min3 :: (Ord a) => [a] -> Maybe a
min3 [x] = Just x
min3 (x:y:xs) = min3 $ (min x y) : xs
min3 _ = Nothing

max3 :: (Ord a) => [a] -> Maybe a
max3 [x] = Just x
max3 (x:y:xs) = max3 $ (max x y) : xs
max3 _ = Nothing

-- Version 4 Using Case syntax

min4 :: (Ord a) => [a] -> Maybe a
min4 l = case l of
           [x] -> Just x
           (x:y:xs) -> min4 $ min x y : xs
           _ -> Nothing

max4 :: (Ord a) => [a] -> Maybe a
max4 l = case l of
           [x] -> Just x
           (x:y:xs) -> max4 $ max x y : xs
           _ -> Nothing

