{-# LANGUAGE TupleSections #-}
module Main ( main ) where 

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict ( Map(..) )
import Data.Set ( Set(..) )
import Data.List ( sort )
import Data.Monoid ( Sum(..) )
import System.Random 
import Rec 

type Key = String
type Graph = Map Key [Key]

parse :: [String] -> Graph
parse            = Map.unionsWith (<>) . map parseLine
parseLine        = makeEdges . words . filter (/=':')
makeEdges (x:xs) = Map.singleton x xs <> Map.fromList [(y, [x]) | y <- xs]

contract :: Key -> Key -> Graph -> Graph
contract u v g = Map.update (\xs -> Just (filter (/=uv) xs)) uv
               $ fmap (\xs ->  map (\x -> if x == v then uv else if x == u then uv else x) xs) 
               $ Map.insert uv (g Map.! u <> g Map.! v)
               $ Map.delete u $ Map.delete v 
               $ g
 where uv = u <> " " <> v

pickEdge seed g = let ks = Map.keys g
                      (ix, seed')  = randomR (0, length ks - 1) seed 
                      u            = ks !! ix
                      xs           = filter (`Map.member` g) $ g Map.! u
                      (iy, seed'') = randomR (0, length xs - 1) seed' 
                   in ((u, xs !! iy), seed')

isSol = (==3) . length . head . Map.elems

karger :: (Graph, StdGen) -> Int
karger (graph, seed') = hylo delayed coalg (graph, seed')
  where 
    coalg (g, seed) 
      | Map.size g == 2 = if isSol g 
                             then Value $ foldr (\k acc -> acc * (length . words) k) 1 $ Map.keys g 
                             else Delayed (graph, seed)
      | otherwise       = let ((u, v), seed') = pickEdge seed g
                           in Delayed (contract u v g, seed')

main :: IO ()
main = do seed <- getStdGen 
          karger . (,seed) . parse . lines <$> readFile "inputs/2023/input25.txt"  
         >>= print 
