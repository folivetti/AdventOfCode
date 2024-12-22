{-# LANGUAGE TupleSections #-}
module Main where

import Data.List
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M 


type Coord = (Int, Int)

toCoord :: Char -> Coord
toCoord '0' = (1, 3)
toCoord 'A' = (2, 3)
toCoord '1' = (0, 2)
toCoord '2' = (1, 2)
toCoord '3' = (2, 2)
toCoord '4' = (0, 1)
toCoord '5' = (1, 1)
toCoord '6' = (2, 1)
toCoord '7' = (0, 0)
toCoord '8' = (1, 0)
toCoord '9' = (2, 0)
toCoord 'a' = (2, 0)
toCoord '^' = (1, 0)
toCoord '<' = (0, 1)
toCoord 'v' = (1, 1)
toCoord '>' = (2, 1)

keys = "0A123456789a<v>^"

path :: Char -> Char -> [String]
path from to 
  | from == '<' || x1 == 0 && y2 == 3 = [reverse ('a':moves)]
  | to == '<'   || x2 == 0 && y1 == 3 = [moves <> "a"]
  | otherwise = nub [reverse ('a':moves), moves <> "a"]
  where
    (x1, y1) = toCoord from
    (x2, y2) = toCoord to
    moves = replicate (abs (y2 - y1)) (if y2 > y1 then 'v' else '^') <> replicate (abs (x2 - x1)) (if x2 > x1 then '>' else '<')


complexity :: M.Map (Char, Char) [String] -> Int -> String -> Int
complexity paths m code = evalState (find (m+1) 'A' code) M.empty * read (init code)
  where
      find :: Int -> Char -> String -> State (M.Map (Int, Char, Char) Int) Int
      find 0 c s = pure $ length s
      find n c s = foldM (go n) 0 $ zip (c:s) s

      go n tot (from, to) = do cache <- gets ((n, from, to) `M.lookup`)
                               case cache of
                                 Just x -> pure (tot + x)
                                 Nothing -> do
                                     x <- case paths M.! (from, to) of
                                            [path] -> find (n-1) 'a' path
                                            [path1, path2] -> do
                                                x1 <- find (n-1) 'a' path1
                                                x2 <- find (n-1) 'a' path2
                                                pure $ min x1 x2
                                     modify (M.insert (n, from, to) x)
                                     pure (tot + x)

f &&& g = \x -> (f x, g x)

solve = (sum . map (complexity paths 2)) &&& (sum . map (complexity paths 25))
  where
      paths = M.fromList [((from, to), path from to) | from <- keys, to <- keys]

main :: IO ()
main = solve . words <$> readFile "inputs/2024/input21.txt"
         >>= print
