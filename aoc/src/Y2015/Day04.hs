module Y2015.Day04 (solution) where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)

hash :: String -> String
hash = unpack . encode . MD5.hash . pack

findSol :: String -> Int -> Int
findSol pat n = go 1
  where
    go i
      | hasFive i = i
      | otherwise = go (i+1)
    hasFive = (==pat) . take n . hash . ("iwrupvqb" <>) . show

solution :: IO ()
solution = do
  print $ findSol "00000" 5
  print $ findSol "000000" 6
