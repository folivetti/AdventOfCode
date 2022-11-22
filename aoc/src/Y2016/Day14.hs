module Y2016.Day14 ( solution ) where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)
import Data.List ( isInfixOf )

hash, stretchHash :: String -> String
hash = unpack . encode . MD5.hash . pack
stretchHash = unpack . (!! 2017) . iterate (encode . MD5.hash) . pack
{-# inline hash #-}

hashFromDigit :: Int -> String
hashFromDigit = hash . ("cuanljph" <>) . show
{-# inline hashFromDigit #-}

stretchHashFromDigit :: Int -> String
stretchHashFromDigit = stretchHash . ("cuanljph" <>) . show

allHashes :: [String]
allHashes = go 0
  where
    go i = hashFromDigit i : go (i+1)
{-# inline allHashes #-}

allStretchHashes :: [String]
allStretchHashes = go 0
  where
    go i = stretchHashFromDigit i : go (i+1)

findTriple :: String -> Maybe Char
findTriple (x:y:z:xs) 
  | x == y && y == z = Just x
  | otherwise        = findTriple (y:z:xs)
findTriple _         = Nothing
{-# inline findTriple #-}

getKeys :: [(Int, String)] -> [(Int, String)]
getKeys [] = []
getKeys ((ix, x):xs) = case findTriple x of
                         Nothing -> getKeys xs
                         Just c  -> if any (([c,c,c,c,c] `isInfixOf`) . snd) (take 1000 xs)
                                      then (ix, x) : getKeys xs
                                      else getKeys xs


solution :: IO ()
solution = do
  print $ (getKeys  $ zip [0..] allHashes) !! 63
  print $ (getKeys  $ zip [0..] allStretchHashes) !! 63
