module Main where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)
import Data.List ( sort, nubBy )
import Control.Arrow ( (&&&) )

hash :: String -> String
hash = unpack . encode . MD5.hash . pack
{-# inline hash #-}

hashFromDigit :: Int -> String
hashFromDigit = hash . ("wtnhxymk" <>) . show
{-# inline hashFromDigit #-}

allHashes :: [String]
allHashes = go 0
  where
    go i = hashFromDigit i : go (i+1)
{-# inline allHashes #-}

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith pat = and . zipWith (==) pat
{-# inline startsWith #-}

part1 :: [String] -> String
part1 = take 8 . map (!! 5) . filter (startsWith "00000")

part2 :: [String] -> String
part2 = map last . sort . map (take 2) . take 8 . nubBy eqFst . map (drop 5) . filter isValid
  where
    eqFst x y = head x == head y
    isValidIx = (`elem` "01234567")
    isValid x = startsWith "00000" x && isValidIx (x !! 5)

main :: IO ()
main = do
  putStrLn $ part1 allHashes
  putStrLn $ part2 allHashes
