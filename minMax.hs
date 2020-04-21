-- Find minimum and maximum from given list

l :: [Int]
l = [1, 2, 3, 4, 5, 6, 7];


-- Version 1

min' [] = 0
min' (x:xs) = min'' x xs
min'' n [] = n
min'' n (x:xs) = if x < n then min'' x xs else min'' n xs

max' [] = 0
max' (x:xs) = max'' x xs
max'' n [] = n
max'' n (x:xs) = if x > n then max'' x xs else max'' n xs

main = do
  print ("For list : " ++ show l)
  print ("Minimum = " ++ (show (min' l)))
  print ("Maximum = " ++ (show (max' l)))
