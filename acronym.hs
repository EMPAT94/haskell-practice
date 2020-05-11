-- Create acronyms for input phrases
-- eg Portable Network Graphics => PNG

import qualified Data.Char as C

main :: IO ()
main = do
  print "--- START ---"
  print ("Portable Network Graphics => " ++ acronym "Portable Network Graphics") -- PNG
  print ("Metal-oxide Semiconductor => " ++ acronym "Metal-oxide Semiconductor")-- MOS
  print ("GNU Image Manipulation Program => " ++ acronym "GNU Image Manipulation Program") -- GIMP
  print ("HyperText Transfer Protocol => " ++ acronym "HyperText Transfer Protocol") -- HTTP
  print ("__Test with underscores__ => " ++ acronym "__Test with underscores__") -- TWU
  print "--- END ---"


acronym = concatMap getInitials . words . map (\l -> if l `elem` "-_" then ' ' else l)

getInitials word
  | null word = []
  | all C.isUpper word = [head word]
  | otherwise = C.toUpper (head word) : filter C.isUpper (tail word)
