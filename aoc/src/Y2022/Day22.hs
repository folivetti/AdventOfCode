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
data Dir = N | S | E | W deriving (Show)

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
walk cube (eRows, eCols) dir n = go n
  where
    getWarp E (x, y) = (x,) . fst $ eRows M.! x
    getWarp W (x, y) = (x,) . snd $ eRows M.! x
    getWarp S (x, y) = (,y) . fst $ eCols M.! y
    getWarp N (x, y) = (,y) . snd $ eCols M.! y

    addDir E (x, y) = (x, y+1)
    addDir W (x, y) = (x, y-1)
    addDir S (x, y) = (x+1, y)
    addDir N (x, y) = (x-1, y)
    
    go 0 xy = xy
    go m xy = case cube M.!? xy' of
                Nothing -> let warped = getWarp dir xy 
                            in trace (show (xy,dir,xy',warped)) $ if cube M.! warped == '.'
                                 then go (m-1) warped
                                 else xy
                Just '.' -> go (m-1) xy'
                Just '#' -> xy
                _        -> error "where are you going, mate?"
      where
        xy' = addDir dir xy

follow :: (Dir -> Int -> Coord -> Coord) -> (Coord, Dir) -> Either Int Char -> (Coord, Dir)
follow walker (coord, dir) (Left n) = (,dir) $ walker dir n coord
follow walker (coord, dir) (Right r) = (coord, ) $ rot r dir

part1 ((x,y), dir) = 1000 * (x + 1) + 4 * (y + 1) + cost dir
  where
      cost E = 0
      cost S = 1
      cost W = 2
      cost N = 3

main :: IO ()
main = do content <- lines <$> readFile "inputs/2022/input22.txt"
          let (cube, instructions) = parseMap content
              (eRows, eCols) = entries content
              walker = walk cube (eRows, eCols)
              s0 = (0,) $ fst $ eRows M.! 0
          print eRows
          print eCols
          print $ part1 $ foldl (follow walker) (s0,E) instructions
