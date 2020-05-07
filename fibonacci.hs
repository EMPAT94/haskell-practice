--  Fibonnaci Series [1, 1, 2, 3, 4, 8, ...]

-- Version 1 
fib :: Integer -> [Integer]
fib n = reverse $ fib' n [1, 1]

fib' :: Integer -> [Integer] -> [Integer]
fib' n s 
  | n > 2 = fib' (n-1) (head s + head (tail s) : s)
  | otherwise = s
