{-# language OverloadedStrings #-}

module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import Data.List ( span )
import Data.Bifunctor ( bimap )
import Debug.Trace ( traceShow ) 
import Control.Monad ( guard )
import Control.Arrow ( (&&&) )

data Result = Jmp String | Accept | Reject deriving (Show, Eq)

parser :: ([B.ByteString], [B.ByteString]) -> (Map.Map [Char] [(Maybe (Char, Integer, Ordering), Main.Result)], [Map.Map Char Integer])
parser = bimap (Map.unions . map (runParser parseInstructions)) (map (runParser parseData) . tail)
   where 
     parseInstructions = do key <- many' letter_ascii
                            char '{'
                            f <- (parseFun <|> parseFinal) `sepBy` char ','
                            char '}'
                            pure $ Map.singleton key f
     parseFinal  = do res <-  (char 'A' >> pure Accept) <|> (char 'R' >> pure Reject) <|> (Jmp <$> many' letter_ascii)
                      pure (Nothing, res)
     parseFun = do k <- letter_ascii 
                   cmp <- parseLT <|> parseGT
                   v <- decimal -- (Val <$> decimal) <|> (char 'A' >> pure Accept) <|> (char 'R' >> pure Reject)
                   char ':'
                   to <- (char 'A' >> pure Accept) <|> (char 'R' >> pure Reject) <|> (Jmp <$> many' letter_ascii)
                   pure (Just (k, v, cmp), to)
     parseLT = char '<' >> pure LT
     parseGT = char '>' >> pure GT
     parseData = do char '{'
                    kv <- parseKV `sepBy` char ','
                    char '}'
                    pure $ Map.fromList kv
     parseKV = do k <- letter_ascii 
                  char '='
                  v <- decimal
                  pure (k, v)

follow is ds = hylo alg coalg "in"
  where
    alg (Value x) = case x of 
                      Accept -> sum ds 
                      _      -> 0
    alg (Delayed xs) = xs 

    coalg k = case eval (is Map.! k) of 
                Jmp k' -> Delayed k'
                x      -> Value x 

    eval []           = Reject
    eval ((f, to):fs) = if predicate f then to else eval fs

    predicate Nothing           = True
    predicate (Just (k, v, LT)) = ds Map.! k < v
    predicate (Just (k, v, GT)) = ds Map.! k > v

solve1 (is, ds) = sum $ map (follow is) ds  

solve2 :: (Map.Map String [(Maybe (Char, Integer, Ordering), Main.Result)], [Map.Map Char Integer]) -> Integer
solve2 (is, _) = go (is Map.! "in") s0 -- [(Jmp "in", s0)]
  where
    s0       = Map.fromList [(c, (1, 4000)) | c <- "xmas"]
    sizeOf s = product $ fmap (\(lo, hi) -> hi - lo + 1) s

    go ((Nothing, to) : _) s = case to of 
                                  Accept -> sizeOf s 
                                  Reject -> 0 
                                  Jmp k  -> go (is Map.! k) s
    go ((Just (k, v, LT), to):xs) s = (if fst (s Map.! k) < v 
                                         then case to of 
                                                Accept -> sizeOf $ Map.adjust (\(lo,hi) -> (lo, min (v-1) hi)) k s
                                                Reject -> 0
                                                Jmp k' -> go (is Map.! k') $ Map.adjust (\(lo,hi) -> (lo, min (v-1) hi)) k s
                                         else 0) +
                                      (if snd (s Map.! k) >= v 
                                         then go xs $ Map.adjust (\(lo,hi) -> (max lo v, hi)) k s
                                         else 0)
    go ((Just (k, v, GT), to):xs) s = (if snd (s Map.! k) > v 
                                         then case to of 
                                                Accept -> sizeOf $ Map.adjust (\(lo,hi) -> (max lo (v+1), hi)) k s 
                                                Reject -> 0 
                                                Jmp k' -> go (is Map.! k') $ Map.adjust (\(lo,hi) -> (max lo (v+1), hi)) k s
                                         else 0) +
                                      (if fst (s Map.! k) <= v 
                                         then go xs $ Map.adjust (\(lo,hi) -> (lo, min hi v)) k s
                                         else 0)
    
main :: IO ()
main = solve2 . parser . span((/="")) . B.lines <$> B.readFile "inputs/2023/input19.txt"
         >>= print
