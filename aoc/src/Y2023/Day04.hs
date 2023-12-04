
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as Set 
import qualified Data.Map.Strict as Map

parser :: Parser (Int, Set.Set Int, Set.Set Int)
parser = do string "Card"
            many' space
            n <- decimal
            string ":" >> many' space
            winners <- decimal `sepBy` (many' (char ' '))
            many' space >> string "|" >> many' space 
            card <- decimal `sepBy` (many' (char ' '))
            pure $ (n, Set.fromList winners, Set.fromList card)

solve = post . cata sumScores . fromList
  where
    post (p1, p2)                                 = (p1, sum p2)
    sumScores NilF                                = (0, mempty)
    sumScores (ConsF (n, winners, card) (p1, p2)) = (p1 + score, p2')
      where
        wins     = Set.size $ Set.intersection winners card
        score    = if wins == 0 
                      then 0 
                      else 2 ^ (wins - 1)
        p2'      = Map.insert n (1 + sum next) p2
        next     = Map.filterWithKey (\k _ -> k > n && k <= n + wins) p2

main :: IO ()
main = solve . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input04.txt"
         >>= print
