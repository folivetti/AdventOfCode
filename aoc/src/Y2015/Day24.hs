module Main where

pack :: [Int] -> Int -> Int
pack ys n = minimum $ go ys (sum ys `div` n) (length ys `div` n)
  where
    go _ 0 _      = [1]
    go [] _ _     = []
    go _ _ 0      = []
    go (x:xs) v s = map (x*) (go xs (v-x) (s-1)) <> go xs v s

main :: IO ()
main = do content <- lines <$> readFile "inputs/2015/input24.txt" 
          let gifts = reverse $ map read content :: [Int]
          print $ pack gifts 3
          print $ pack gifts 4
