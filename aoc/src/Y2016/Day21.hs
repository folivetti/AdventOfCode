{-# language OverloadedStrings #-}
module Y2016.Day21 ( solution ) where

import Data.Attoparsec.ByteString.Char8 hiding ( take )
import qualified Data.ByteString.Char8 as B
import Control.Applicative ( (<|>) )
import Utils ( runParser )
import Data.List ( elemIndex, foldl', permutations, find )

parser :: Parser (String -> String)
parser = swapPos <|> swapLetter <|> rotLeft <|> rotRight <|> rotLetter <|> reversePos <|> movePos

swapPos :: Parser (String -> String)
swapPos = do string "swap position "
             x <- decimal
             string " with position "
             y <- decimal
             pure $ \xs -> let vx = xs !! x
                               vy = xs !! y
                            in zipWith (\ix v -> if ix == x then vy
                                                 else if ix == y then vx
                                                 else v) [0..] xs

swapLetter :: Parser (String -> String)
swapLetter = do string "swap letter "
                x <- anyChar
                string " with letter "
                y <- anyChar
                pure $ map (\c -> if c==x then y else if c==y then x else c)

rotLeft :: Parser (String -> String)
rotLeft = do string "rotate left "
             n <- (`mod` tot) <$> decimal
             string " steps" <|> string " step"
             pure $ \xs -> drop n xs <> take n xs

rotRight :: Parser (String -> String)
rotRight = do string "rotate right "
              n <- (`mod` tot) <$> decimal
              string " steps" <|> string " step"
              pure $ \xs -> drop (tot - n) xs <> take (tot - n) xs

rotLetter :: Parser (String -> String)
rotLetter = do string "rotate based on position of letter "
               x <- anyChar
               pure $ \xs -> let ix = fromJust $ elemIndex x xs
                                 n  = 1 + ix + if ix >= 4 then 1 else 0
                              in drop (tot - n `mod` tot) xs <> take (tot - n `mod` tot) xs

fromJust (Just x) = x

reversePos :: Parser (String -> String)
reversePos = do string "reverse positions "
                x <- decimal
                string " through "
                y <- decimal
                pure $ \xs -> let (l', r) = splitAt (y+1) xs
                                  (l, m)  = splitAt x l'
                               in l <> reverse m <> r

movePos :: Parser (String -> String)
movePos = do string "move position "
             x <- decimal
             string " to position "
             y <- decimal
             pure $ \xs -> insertAt y (xs !! x) $ deleteAt x xs

insertAt :: Int -> a -> [a] -> [a]
insertAt ix x xs = take ix xs <> (x : drop ix xs)
deleteAt :: Int -> [a] -> [a]
deleteAt ix xs = take ix xs <> tail (drop ix xs)

solve :: String -> [String -> String] -> String
solve = foldl' (flip ($))

tot :: Int
tot = 8

solution :: IO ()
solution = do content <- map (runParser parser) . B.lines <$> B.readFile "inputs/2016/input21.txt"
              putStrLn $ solve "abcdefgh" content
              print $ find (\x -> solve x content == "fbgdceah") $ permutations "abcdefgh"
