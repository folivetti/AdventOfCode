module Y2016.Day07 ( solution ) where

import Utils ( runParser )
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

parser :: Parser ([String], [String])
parser = do xs <- many1' $ eitherP parseBracket parseChars
            pure $ foldr fromEither ([], []) xs
  where 
      parseBracket = char '[' >> many1' letter_ascii >>= \xs -> char ']' >> pure xs
      parseChars   = many1' letter_ascii
      fromEither (Left xs)  (a, b) = (xs : a, b)
      fromEither (Right xs) (a, b) = (a, xs : b)

supportsTLS :: ([String], [String]) -> Bool
supportsTLS (brackets, nonbrackets)
  | any hasABBA brackets    = False
  | any hasABBA nonbrackets = True
  | otherwise               = False
  where
      hasABBA                = any isPalindrome . sublistsOf
      isPalindrome [a,b,c,d] = a == d && b == c && a /= b
      isPalindrome _         = False
      sublistsOf             = filter ((==4) . length) . map (Prelude.take 4) . scanr (:) []

supportsSSL :: ([String], [String]) -> Bool
supportsSSL (brackets, nonbrackets)
  | any hasABA nonbrackets = True
  | otherwise              = False
  where
      hasABA        = any isABA . sublistsOf
      isABA [a,b,c] = a==c && a/=b && any (isBAB [b,a,b]) brackets
      isABA _       = False
      isBAB xs      = elem xs . sublistsOf
      sublistsOf    = filter ((==3) . length) . map (Prelude.take 3) . scanr (:) []

solution :: IO ()
solution = do
    content <- B.lines <$> B.readFile "inputs/2016/input07.txt"
    let myDat = map (runParser parser) content
    print . length . filter supportsTLS $ myDat
    print . length . filter supportsSSL $ myDat
