--- Factorial n = n! = n * (n-1) * (n-2) ... 1

-- Version 1
factorial :: Int -> Int
factorial n 
  | n < 1 = 1
  | otherwise = n * factorial (n - 1)

