{-# language TupleSections #-}
module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ( find, sortOn )

type Valley = M.Map (Int, Int) String

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

withoutBorder :: [a] -> [(Int, a)]
withoutBorder = enumerate . init . tail

getRows :: String -> [(Int, String)]
getRows = withoutBorder . lines

getCols :: (Int, String) -> [((Int, Int), String)]
getCols (x, css) = [((x, y), [c]) | (y, c) <- withoutBorder css, c /= '.']

parseValley :: String -> Valley
parseValley = M.fromList . concatMap getCols . getRows

updateValley :: (Int, Int) -> Valley -> Valley
updateValley (maxX, maxY) valley = M.fromListWith (<>) $ concatMap (uncurry upd) $ M.toList valley
  where
    upd coord       = map (move coord)
    move (x, y) '>' = ((x, (y + 1) `mod` maxY), ">")
    move (x, y) '<' = ((x, (y - 1) `mod` maxY), "<")
    move (x, y) '^' = (((x - 1) `mod` maxX, y), "^")
    move (x, y) 'v' = (((x + 1) `mod` maxX, y), "v")

astar :: (Int, Int) -> M.Map Int Valley -> ((Int, Int), Int) -> (Int, Int) -> Int
astar (maxX, maxY) valleys start end = go S.empty [start]
  where
    go _ [] = error "all invalid states"
    go seen (st@((x, y), t):xs)
      | (x, y) == end      = t
      | st `S.member` seen = go seen xs
      | otherwise          = go (S.insert ((x,y), t) seen) xs'
      where 
        xs'              = sortOn heur (xs <> map (,t+1) (next (x,y)))
        cyc              = (t + 1) `mod` lcm maxY maxX
        valley           = valleys M.! cyc
        next (a, b)      = filter valid [(a+1, b), (a-1, b), (a, b+1), (a, b-1), (a, b)]
        inBound (a, b)   = (a, b) == fst start || (a, b) == end || (a >= 0 && a < maxX && b >= 0 && b < maxY)
        valid (a, b)     = ((a, b), cyc) `S.notMember` seen && inBound (a, b) && (a, b) `M.notMember` valley
        heur ((a,b), t') = t' + abs (a - fst end) + abs (b - snd end)

main :: IO ()
main = do valley <- parseValley <$> readFile "inputs/2022/input24.txt"
          let valleySize   = (\(x,y) -> (x+1, y+1)) $ maximum $ M.keys valley
              allValleys   = M.fromList $ enumerate $ take (uncurry lcm $ valleySize) $ iterate (updateValley valleySize) valley
              (start, end) = ((-1, 0), (fst valleySize, snd valleySize - 1))
              t1 = astar valleySize allValleys (start, 0) end
              t2 = astar valleySize allValleys (end, t1) start
              t3 = astar valleySize allValleys (start, t2) end
          print t1
          print t3
