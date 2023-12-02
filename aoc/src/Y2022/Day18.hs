module Main where

import Data.Set ( Set, fromList, toList, notMember, member, (\\), empty, union )
import qualified Data.Set as S

type Triple = (Int, Int, Int)

toTriple :: [a] -> (a, a, a)
toTriple [x,y,z] = (x,y,z)
toTriple _ = error "malformed list"

adjacent :: Triple -> [Triple]
adjacent (x, y, z) = [ (x+1, y, z), (x-1, y, z)
                     , (x, y+1, z), (x, y-1, z)
                     , (x, y, z+1), (x, y, z-1)
                     ]

countIf :: (Triple -> Set Triple -> Bool) -> Set Triple -> Triple -> Int
countIf p cubes = length . filter (`p` cubes) . adjacent

getExterior :: Set Triple -> Set Triple
getExterior cubes = let (x, y, z) = minimum cubes
                     in flood empty $ fromList [(x-1, y, z)]
  where
      faces  = any (`member` cubes) . adjacent
      hasAdj = any faces . adjacent

      flood seen curFront
        | S.null curFront = seen
        | otherwise       = flood seen' curFront'
          where seen'     = seen `union` curFront
                curFront' = fromList [ adj | cube <- toList curFront
                                           , adj <- adjacent cube
                                           , adj `notMember` seen
                                           , adj `notMember` cubes
                                           , faces adj || hasAdj adj
                                     ]

main :: IO ()
main = do content <- map (toTriple . map read . words) . lines <$> readFile "inputs/2022/input18.txt"
          let cubes = fromList content
              sizeWith p = foldr (\c acc -> acc + countIf p cubes c) 0
          print $ sizeWith notMember cubes 
          print $ sizeWith member $ getExterior cubes 
