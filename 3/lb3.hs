import System.IO
import Data.List

groupC :: Int
groupC = 5
text = "FA0ETASINAHGRI0NATWON0QA0NARI0"
lengthG = length text `div` groupC
textG = groupD lengthG text
textG3 = concat (map reverse textG)

main = writeFile "result.txt" textG3

groupD :: Int -> [a] -> [[a]]
groupD _ [] = []
groupD n l
  | n > 0 = (take n l) : (groupD n (drop n l))
  | otherwise = error "Negative or zero n"