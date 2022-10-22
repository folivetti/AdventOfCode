module Y2015.Day05 (solution) where

part1, part2 :: String -> Bool
part1 str = go str (0, False)
  where
      naughty = ["ab", "cd", "pq", "xy"]
      isVowel = fromEnum . (`elem` "aeiou")
      go [] (b1, b2) = b1 >= 3 && b2
      go [x] (b1, b2) = (b1 + isVowel x >= 3) && b2
      go (x:y:xs) (b1, b2)
        | [x,y] `elem` naughty = False
        | otherwise = go (y:xs) (b1 + isVowel x, b2 || x==y)

part2 str = go str (False, False)
  where
      twice xy xs = xy `elem` zip xs (tail xs)
      go (x:y:z:xs) (b1, b2) = go (y:z:xs) (b1 || x==z, b2 || twice (x,y) (z:xs))
      go (x:y:xs) (b1, b2)   = go (y:xs) (b1, b2 || twice (x,y) xs)
      go _ (b1, b2)          = b1 && b2

solution :: IO ()
solution = do
  content <- readFile "inputs/2015/input05.txt"
  let isNice f = length . filter f . lines
  print $ isNice part1 content
  print $ isNice part2 content
