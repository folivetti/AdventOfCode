{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.List ( sortBy, sort, inits, tails )
import qualified Data.Map as Map 
import qualified Data.Set as Set 
import Data.Map ( Map ) 
import Data.Set ( Set ) 
import Data.Ord ( comparing, Down(..) )

type Coord = (Int, Int) 
data Brick = Brick { _z :: Coord, _x :: Coord, _y :: Coord } deriving (Show, Eq, Ord)

parser :: Parser Brick
parser = do x1 <- decimal
            char ','
            y1 <- decimal
            char ','
            z1 <- decimal 
            char '~'
            x2 <- decimal 
            char ','
            y2 <- decimal
            char ','
            z2 <- decimal
            pure $ Brick (min z1 z2, max z1 z2) (min x1 x2, max x1 x2) (min y1 y2, max y1 y2)

sortDesc = sortBy (comparing Down) 
(a, b) .-. (c, d) = (a-c, b-d)
hasIntersection (x, y) (z, w) = max x z <= min y w 
hasIntersectionBrick brick1 brick2 =  hasIntersection (_z brick1) (_z brick2) 
                             && hasIntersection (_x brick1) (_x brick2) 
                             && hasIntersection (_y brick1) (_y brick2)
noIntersection brick brickes = all (not . hasIntersectionBrick brick) brickes 
down x 
  | fst (_z x) == 1 = x 
  | otherwise       = x{ _z = _z x .-. (1, 1) }

dropBrick :: [Brick] -> [Brick]
dropBrick = cata alg . fromList -- foldr putDown []
  where
    alg NilF = []
    alg (ConsF x xs) = putDown x xs : xs

putDown x xs = hylo alg coalg x 
  where
    alg (Value y) = y 
    alg (Delayed y) = y 

    coalg y = let y' = down y
               in if y /= y' && noIntersection y' xs
                    then Delayed y' 
                    else Value y

drops :: [Brick] -> [Brick] -> Int
drops bricks = fst . cata alg . fromList
  where
    alg NilF              = (0, bricks)
    alg (ConsF x (n, xs)) = let x' = down x
                              in if x /= x' && noIntersection x' xs 
                                   then (n+1, xs) 
                                   else (n, x:xs)

solve :: [Brick] -> (Int, Int)
solve bricks = cata alg . fromList $ zip (inits bricks) (tails bricks)
  where
    alg NilF                        = (0, 0)
    alg (ConsF (ins, []) (p1, p2))  = (p1, p2)
    alg (ConsF (ins, tls) (p1, p2)) = let count = drops ins (reverse (tail tls))
                                       in (p1 + if count==0 then 1 else 0, p2 + count)

main :: IO () 
main = solve . sort. dropBrick . sortDesc . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input22.txt"
            >>= print 
