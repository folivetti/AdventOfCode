module Main ( main ) where

import Data.Char ( isDigit, digitToInt )
import Data.List ( isPrefixOf )
import Data.Monoid ( Sum(..) )
import Rec 
import Utils

extractDigits :: String -> (Sum Int, Sum Int)
extractDigits = both (Sum . fixDigit) . histo alg . fromList 
  where 
    -- if there is a single digit you must do xx
    fixDigit x = if x < 10 
                   then x * 10 + x
                   else x

    replace x y = if y == 0 
                    then x 
                    else x * 10 + mod y 10

    alg NilF          = (0, 0)
    alg (ConsF x table)
      | isDigit x = let d = digitToInt x 
                     in (replace d xs, replace d ys)
      | otherwise = case convert x table of 
                      Nothing -> (xs, ys)
                      Just d  -> (xs, replace d ys)
      where 
          (xs, ys) = extract table

convert :: Char -> Cofree (ListF Char) a -> Maybe Int
convert x table 
  | lenCF table == 0 = Nothing 
  | otherwise        = let xs = takeCF 4 table
                        in case (x:xs) of
                             'o':'n':'e':_         -> Just 1 
                             't':'w':'o':_         -> Just 2 
                             't':'h':'r':'e':'e':_ -> Just 3 
                             'f':'o':'u':'r':_     -> Just 4
                             'f':'i':'v':'e':_     -> Just 5
                             's':'i':'x':_         -> Just 6
                             's':'e':'v':'e':'n':_ -> Just 7
                             'e':'i':'g':'h':'t':_ -> Just 8
                             'n':'i':'n':'e':_     -> Just 9
                             _                     -> Nothing

solve :: [String] -> (Sum Int, Sum Int)
solve = cata alg . fromList
  where
    alg NilF          = mempty
    alg (ConsF x acc) = extractDigits x <> acc

main :: IO ()
main = both getSum . solve . lines <$> readFile "inputs/2023/input01.txt"
         >>= print
