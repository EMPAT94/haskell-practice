-- Anagrams https://en.wikipedia.org/wiki/Anagram
--
-- Following programs takes a word and a list of words as input
-- and returns all the words from the list which are anagrams of
-- the first argument word
--

import Data.Char (toLower)
import Data.List (sort)

main :: IO ()
main = runTest test

runTest :: [(String, [String])] -> IO ()
runTest [] = return ()
runTest ((w,l):rest) = do
  print ("Word : " ++ w ++ ", List : " ++ show l ++ ", Anagrams : " ++ show (anagramsFor w l))
  runTest rest

anagramsFor :: String -> [String] -> [String]
anagramsFor [] [] = []
anagramsFor [] _ = []
anagramsFor _ [] = []
anagramsFor mainWord (testWord:rest)
  | mainWord' /= testWord' && (sort mainWord') == (sort testWord') = testWord : anagramsFor mainWord rest
  | otherwise = anagramsFor mainWord rest
  where
    mainWord' = map toLower mainWord
    testWord' = map toLower testWord

test = [
        ("diaper", [ "hello", "world", "zombies", "pants"]),
        ("master",["stream", "pigeon", "maters"]),
        ("good",["dog", "goody"]),
        ("listen",["enlists", "google", "inlets", "banana"]),
        ("allergy",["gallery", "ballerina", "regally", "clergy", "largely", "leading"]),
        ("mass",["last"]),
        ("Orchestra",["cashregister", "Carthorse", "radishes"]),
        ("Orchestra", ["cashregister", "carthorse", "radishes"]),
        ("orchestra",["cashregister", "Carthorse", "radishes"]),
        ("go",["go Go GO"]),
        ("tapper",["patter"]),
        ("BANANA",["BANANA", "Banana", "banana"])
       ]
