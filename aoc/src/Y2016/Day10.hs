{-# language OverloadedStrings #-}
module Main where

import Utils ( runParser )
import Data.Attoparsec.ByteString.Char8 ( decimal, string, eitherP, Parser )
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import Control.Monad ( foldM, when )
import Data.List ( sort )

data Stat  = Stat { _robot  :: M.IntMap [Int]
                  , _output :: M.IntMap Int
                  , _edges  :: M.IntMap (Either Int Int, Either Int Int)
                  } deriving Show

parser :: Stat -> Parser Stat
parser stat = do valOrEdge <- eitherP parseVal parseEdge
                 case valOrEdge of
                   Left  x -> pure $ insertVal x
                   Right x -> pure $ insertEdge x
  where
    insertVal (bot, val)   = stat { _robot = M.insertWith (++) bot [val] (_robot stat) }
    insertEdge (bot, lohi) = stat { _edges = M.insert bot lohi (_edges stat) }

parseVal :: Parser (Int, Int)
parseVal = do string "value "
              chip <- decimal
              string " goes to bot "
              bot <- decimal
              pure (bot, chip)

parseEdge :: Parser (Int, (Either Int Int, Either Int Int))
parseEdge = do string "bot "
               bot <- decimal
               string " gives low to "
               low <- botOrOutput
               string " and high to "
               hi <- botOrOutput
               pure (bot, (low, hi))
    where
      botOrOutput = eitherP (string "bot " >> decimal) (string "output " >> decimal)

st0 :: Stat
st0 = Stat M.empty M.empty M.empty

simulate :: Stat -> IO Stat
simulate st
  | M.null (_robot st) = pure st
  | otherwise          = foldM update st (M.toList (_robot st)) 
                          >>= simulate
  where
    update :: Stat -> (Int, [Int]) -> IO Stat
    update s (k,v)
      | length v /= 2 = pure s
      | otherwise = do
          let (lo:hi:_) = sort v
              (toLo, toHi) = _edges s M.! k
          when (lo == 17 && hi == 61) $
            putStrLn $ "Bot " <> show k
          let robots = _robot s
              bins   = _output s
              (r, b) = insertEither toHi hi $ insertEither toLo lo (robots, bins)
          pure $ Stat (M.delete k r) b (_edges s)
    insertEither eval v (r, b) = case eval of
                                   Left x  -> (M.insertWith (++) x [v] r, b)
                                   Right x -> (r, M.insert x v b)


main :: IO ()
main = do content <- B.lines <$> B.readFile "inputs/2016/input10.txt"
          let st = foldr (\x acc -> runParser (parser acc) x) st0 content
          st' <- simulate st
          let outs = _output st'
          print $ outs M.! 0 * outs M.! 1 * outs M.! 2
