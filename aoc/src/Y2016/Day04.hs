{-# language TupleSections #-}
module Y2016.Day04 ( solution ) where

import Utils
import Data.Attoparsec.ByteString.Char8 hiding ( take )
import Data.ByteString.Char8 ( ByteString, pack )
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.List ( sort, intercalate )

parser :: Parser [String]
parser = many' (notChar '-') `sepBy` char '-'

getSectorCheckSum :: String -> (Int, String)
getSectorCheckSum = runParser parseSector . pack

parseSector :: Parser (Int, String)
parseSector = do sector <- decimal
                 char '['
                 checksum <- many' letter_ascii
                 char ']'
                 pure (sector, checksum)

getCodes :: ByteString -> ([String], Int, String)
getCodes xs = let ys = runParser parser xs
                  (sector, checksum) = getSectorCheckSum $ last ys
               in (init ys, sector, checksum)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

sortByFreq :: [String] -> String
sortByFreq = take 5 . map snd . sort . map swap . M.toList . M.fromListWith (+) . map (,-1) . concat

cycleChar :: Int -> Char -> Char
cycleChar _ '-' = ' '
cycleChar n c = toEnum $ start + (fromEnum c - start + n) `mod`  26
  where start = fromEnum 'a'

isValid :: ([String], Int, String) -> Bool
isValid (a, _, c) = sortByFreq a == c

countValids :: [([String], Int, String)] -> Int
countValids = sum . map (\(_,b,_) -> b) . filter isValid

isNorthPole :: ([String], Int, String) -> Bool
isNorthPole = (== "northpole object storage") . decode

decode :: ([String], Int, String) -> String
decode (a, b, _) = map (cycleChar b) a'
  where a' = intercalate "-" a

solution :: IO ()
solution = do content <- B.lines <$> B.readFile "inputs/2016/input04.txt"
              let codes = map getCodes content
                  part1 = countValids codes
                  part2 = map (\(a,b,c) -> b) $ filter isNorthPole $ filter isValid codes
              print part1
              mapM_ print part2
