module Main ( main ) where 

import Utils ( runParser )
import Rec
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Monoid ( Sum(..) )
import Data.Bifunctor ( second )

parser :: Parser [Int]
parser = (signed decimal) `sepBy` space 

-- This can be a histo
step xs = zipWith subtract xs (tail xs) 

extrapolate = hylo alg coalg
  where
    coalg xs
      | any (/=0) xs = ConsF xs (step xs)
      | otherwise    = NilF

    alg NilF = (Sum 0, Sum 0)
    alg (ConsF x xs) = second negate xs <> (Sum (last x), Sum (head x))

solve = cata alg . fromList 
  where
    alg NilF         = (Sum 0, Sum 0)
    alg (ConsF x xs) = extrapolate x <> xs

main :: IO ()
main = solve . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input09.txt"
         >>= print
