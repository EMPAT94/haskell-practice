-- Count number of upper case words in a String
countUpperWords :: String -> Int
countUpperWords = length . filter (isUpper . head) . words 
  where isUpper = (`elem` ['A'..'Z'])

