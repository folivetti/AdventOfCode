{-# language OverloadedStrings #-}
module Y2015.Day16 (solution) where

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Map.Strict as M

runParser :: Parser a -> B.ByteString -> a
runParser parser dat = case parse parser dat of
                  Done _ x -> x
                  Partial p -> case p "" of
                                 Done _ x -> x
                                 _ -> error "no parse"
                  _ -> error "no parse"
{-# INLINE runParser #-}

parseSue :: Parser (Int, [(B.ByteString, Int)])
parseSue = do
  string "Sue "
  n <- decimal
  string ": "
  kvs <- parseKV `sepBy` ", "
  pure (n, kvs)
      where parseKV = do
              k <- P.takeWhile (/=':')
              string ": "
              v <- decimal
              pure (k, v)

mySue :: M.Map B.ByteString Int
mySue = M.fromList [ ("children", 3), ("cats", 7), ("samoyeds", 2)
                   , ("pomeranians", 3), ("akitas", 0), ("vizslas", 0)
                   , ("goldfish", 5), ("trees", 3), ("cars", 2)
                   , ("perfumes", 1)
                   ]

isMySue, isMyTrueSue :: (Int, [(B.ByteString, Int)]) -> Bool
isMySue (_, kvs) = all (\(k,v) -> mySue M.! k == v) kvs
isMyTrueSue (_, kvs) = all cmp kvs
  where
   cmp ("cats", v)        = v > mySue M.! "cats"
   cmp ("trees", v)       = v > mySue M.! "trees"
   cmp ("pomeranians", v) = v < mySue M.! "pomeranians"
   cmp ("goldfish", v)    = v < mySue M.! "goldfish"
   cmp (k, v)             = v == mySue M.! k

solution :: IO ()
solution = do
  content <- B.lines <$> B.readFile "inputs/2015/input16.txt"
  let sues = map (runParser parseSue) content
  print $ filter isMySue sues
  print $ filter isMyTrueSue sues
