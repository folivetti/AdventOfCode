{-# language TupleSections #-}
module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ( find, sortOn )
import qualified Data.OrdPSQ as P

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
astar (maxX, maxY) valleys start end = go S.empty $ P.singleton start (heur start) start
  where
    getCyc t = t `mod` lcm maxX maxY
    heur ((x,y), t') = t' + abs (x - fst end) + abs (y - snd end) 

    go seen st
      | cur == end                = t
      | (cur, t) `S.member` seen  = go seen xs
      | otherwise                 = go (S.insert (cur, t) seen) xs'
      where 
        (Just ((cur, t), _, _, xs)) = P.minView st
        cyc                 = getCyc (t + 1)
        valley              = valleys M.! cyc
        xs'                 = foldr ((\x acc -> P.insert x (heur x) x acc) . (,t+1)) xs $ next cur
        next (a, b)         = filter valid [(a+1, b), (a-1, b), (a, b+1), (a, b-1), (a, b)]
        inBound (a, b  )    = (a, b) == fst start || (a, b) == end || (a >= 0 && a < maxX && b >= 0 && b < maxY)
        valid xy            =  (xy, cyc) `S.notMember` seen 
                            && (xy, t+1) `S.notMember` seen
                            && inBound xy 
                            && xy `M.notMember` valley

main :: IO ()
main = do valley <- parseValley <$> readFile "inputs/2022/input24.txt"
          let valleySize   = (\(x,y) -> (x+1, y+1)) $ maximum $ M.keys valley
              allValleys   = M.fromList $ enumerate $ take (uncurry lcm valleySize) $ iterate (updateValley valleySize) valley
              (start, end) = ((-1, 0), (fst valleySize, snd valleySize - 1))
              t1 = astar valleySize allValleys (start, 0) end
              t2 = astar valleySize allValleys (end, t1) start
              t3 = astar valleySize allValleys (start, t2) end
          print t1
          print t3
