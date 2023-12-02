module Main ( main ) where

import Data.Char ( isDigit, intToDigit )
import Data.List ( isPrefixOf )

replaceNums :: String -> String
replaceNums "" = ""
replaceNums l
  | length (replaceNum l) < length l = replaceNums (replaceNum l)
  | otherwise                        = head l : replaceNums (tail l)

replaceNum :: String -> String
replaceNum xs = foldr f xs dic
  where
    nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    digs = map intToDigit [1..]
    lens = map length nums
    dic  = zip3 nums digs lens

    f (n, d, l) ys = if n `isPrefixOf` ys
                        then d : drop (l-1) ys
                        else ys

extractDigits :: String -> Int
extractDigits  = read . take1stLast . filter isDigit
take1stLast :: [a] -> [a]
take1stLast xs = [head xs, last xs]

main :: IO ()
main = do content <- lines <$> readFile "inputs/2023/input01.txt"
          print . sum . map extractDigits                 $ content
          print . sum . map (extractDigits . replaceNums) $ content
