module Main where

import Data.List ( foldl' )

getContext :: String -> String
getContext (x:y:z:_) = [x,y,z]
getContext [x,y]     = [x,y,'.']
getContext [x]       = [x,'.','.']
getContext []        = "..."

applyRule :: String -> Char
applyRule z
  | getContext z `elem` ["^^.", ".^^", "^..", "..^"] = '^'
  | otherwise = '.'

genNewStr :: String -> String
genNewStr xs = go ('.' : xs)
  where
    go []     = []
    go [_]    = []
    go (z:zs) = applyRule (z:zs) : go zs

myInput :: String
myInput = "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^.."

countSafe :: Int -> [String] -> Int
countSafe n = foldl' (\acc xs -> acc + count xs) 0 . take n
  where count = length . filter (=='.')

main :: IO ()
main = do let rows = iterate genNewStr myInput
          print $ countSafe 40 rows
          print $ countSafe 400000 rows

