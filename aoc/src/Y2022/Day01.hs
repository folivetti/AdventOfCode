module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.List
import Utils ( runParser )

parseElves :: Parser [[Int]]
parseElves = many' parseElf

parseElf :: Parser [Int]
parseElf = do ep <- eitherP decimal endOfLine
              case ep of
                Left x  -> (x:) <$> (do endOfLine; parseElf)
                Right _ -> pure []
    
main :: IO ()
main = do content <- B.readFile "inputs/2022/input01.txt"
          let elves = runParser parseElves content
              totCal = map sum elves
          print $ maximum totCal
          print $ sum $ Prelude.take 3 $ reverse $ sort totCal
