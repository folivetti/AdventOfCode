{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Arrow ( (&&&) )
import Data.List ( transpose )
import Utils ( runParser )
import Debug.Trace ( trace )

type Coord = (Int, Int)
type Entries = M.Map Int Coord
data Dir = E | S | W | N deriving (Show, Enum)

rot :: Char -> Dir -> Dir
rot 'R' N = E
rot 'R' E = S
rot 'R' S = W
rot 'R' W = N
rot 'L' N = W
rot 'L' W = S
rot 'L' S = E
rot 'L' E = N
rot _ _ = error "wrong rot"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

getColumns :: [Char] -> [(Int, Char)]
getColumns = filter ((`elem` (".#" :: String)) . snd) . enumerate

parseInstructions :: Parser [Either Int Char]
parseInstructions = many' $ eitherP decimal anyChar

parseMap :: [String] -> (M.Map Coord Char, [Either Int Char])
parseMap = cube &&& (runParser parseInstructions . B.pack . last)
  where
    cube             = parseMap . init . init
    parseMap         = M.unions . map parseRow . enumerate . map getColumns
    parseRow (i, ys) = M.fromList . map (\(j,c) -> ((i,j), c)) $ ys

entries :: [String] -> (Entries, Entries)
entries (init . init -> xs) = (entriesRows &&& entriesCols) xs
  where
    n                  = length xs
    entriesRows        = M.unions . map entriesRow . enumerate . map getColumns
    entriesRow (i, ys) = M.singleton i . (head &&& last) . map fst $ ys
    adjustIx (i, c)    = (i,c) -- (n-i-1, c)
    swap (a, b)        = (b, a)
    entriesCols        = M.unions . map entriesRow . enumerate . map (map adjustIx . getColumns) . transpose

walk :: M.Map Coord Char -> (Entries, Entries) -> Dir -> Int -> Coord -> Coord
walk cube (eRows, eCols) dir = go
  where
    getWarp E (x, y) = (x,) . fst $ eRows M.! x
    getWarp W (x, y) = (x,) . snd $ eRows M.! x
    getWarp S (x, y) = (,y) . fst $ eCols M.! y
    getWarp N (x, y) = (,y) . snd $ eCols M.! y

    go 0 xy = xy
    go m xy = case cube M.!? xy' of
                Nothing -> let warped = getWarp dir xy 
                            in if cube M.! warped == '.'
                                 then go (m-1) warped
                                 else xy
                Just '.' -> go (m-1) xy'
                Just '#' -> xy
                _        -> error "where are you going, mate?"
      where
        xy' = addDir dir xy

addDir E (x, y) = (x, y+1)
addDir W (x, y) = (x, y-1)
addDir S (x, y) = (x+1, y)
addDir N (x, y) = (x-1, y)
    

follow :: (Dir -> Int -> Coord -> Coord) -> (Coord, Dir) -> Either Int Char -> (Coord, Dir)
follow walker (coord, dir) (Left n) = (,dir) $ walker dir n coord
follow walker (coord, dir) (Right r) = (coord, ) $ rot r dir

warpCube :: (Coord, Dir) -> (Coord, Dir)
warpCube ((0, y), N) 
  | y >= 100  = ((199, y - 100), N)
  | otherwise = ((y + 100, 0), E)
warpCube ((x, 50), W)
  | x < 50  = ((149 - x, 0), E)
  | x < 100 = ((100, x - 50), S)
warpCube ((x, 0), W)
  | x >= 150 = ((0, x - 100), S)
  | x >= 100 = ((149 - x, 50), E)
warpCube ((100, y), N)
  | y < 50 = ((y + 50, 50), E)
warpCube ((149, y), S)
  | y >= 50 = ((y+100, 49), W)
warpCube ((x, 49), E)
  | x >= 150 = ((149, x - 100), N)
warpCube ((199, y), S) = ((0, y + 100), S)
warpCube ((x, 99), E)
  | x >= 100 = ((149 - x, 149), W)
  | x >= 50  = ((49, 50 + x), N)
warpCube ((x, 149), E) = ((149-x, 99), W)
warpCube ((49, y), S)
  | y >= 100 = ((y - 50, 99), W)
warpCube ((x, y), dir) = (addDir dir (x,y), dir)

walk2 :: M.Map Coord Char -> Int -> (Coord, Dir) -> (Coord, Dir)
walk2 cube = go
  where
    go 0 (xy, dir) = (xy, dir)
    go m (xy, dir) = case cube M.!? xy' of
                       Nothing -> let (warped, dir') = warpCube (xy, dir)
                                   in trace (show (xy, dir, xy', warped, dir')) $ if cube M.! warped == '.'
                                       then go (m-1) (warped, dir')
                                       else (xy, dir)
                       Just '.' -> go (m-1) (xy', dir)
                       Just '#' -> (xy, dir)
                       _        -> error "where are you going, mate?"
      where
        xy' = addDir dir xy

follow2 :: M.Map Coord Char -> (Coord, Dir) -> Either Int Char -> (Coord, Dir)
follow2 cube (coord, dir) (Left n)  = walk2 cube n (coord, dir) 
follow2 cube (coord, dir) (Right r) = (coord, rot r dir)

password ((x,y), dir) = 1000 * (x + 1) + 4 * (y + 1) + fromEnum dir

main :: IO ()
main = do content <- lines <$> readFile "inputs/2022/input22.txt"
          let (cube, instructions) = parseMap content
              (eRows, eCols) = entries content
              walker = walk cube (eRows, eCols)
              s0 = (0,) $ fst $ eRows M.! 0
          print $ password $ foldl (follow walker) (s0,E) instructions
          print $ password $ foldl (follow2 cube) (s0,E) instructions
