{-# language OverloadedStrings #-}
module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( replicateM )
import Control.Applicative ( (<|>) )
import Utils ( runParser )
import Data.List ( maximumBy )
import Data.Function ( on )

type Node = (String, (Int, [String]))
type Graph = M.Map String (Int, [String])
data St = St { _cur  :: String
             , _cost :: Int
             , _open :: S.Set String
             } deriving (Show, Eq, Ord)

parser :: Parser Node
parser = do string "Valve "
            k <- replicateM 2 anyChar
            string " has flow rate="
            n <- decimal
            string "; tunnels " <|> string "; tunnel "
            string "lead to " <|> string "leads to "
            string "valves " <|> string "valve "
            vs <- replicateM 2 anyChar `sepBy` string ", "
            pure (k, (n, vs))

step :: Graph -> Int -> St -> [St]
step g t state@(St cur cost open)
  | cur `S.member` open || flow == 0 = walk
  | otherwise                        = openValve:walk
  where
      openValve     = state { _cost = cost + (t-1) * flow, _open = S.insert cur open }
      (flow, nexts) = g M.! cur
      walk          = [ state {_cur = next} | next <- nexts ]

bfs :: Graph -> Int -> St -> M.Map (S.Set String) Int
bfs g t0 st0 = go t0 (uncurry M.singleton (fromState st0))
  where
      fromState (St cur cost open) = ((cur, open), cost)
      toState ((c, o), co) = St c co o
      go 0 sts             = M.mapKeysWith max snd sts
      go t sts             = go (t-1) 
                           $ M.fromListWith max 
                           $ concatMap (map fromState . step g t . toState) 
                           $ M.assocs sts

main :: IO ()
main = do content <- B.lines <$> B.readFile "inputs/2022/input16.txt"
          let g = M.fromList $ map (runParser parser) content
          print $ maximum $ bfs g 30 (St "AA" 0 S.empty)
          let santa          = bfs g 26 (St "AA" 0 S.empty)
              withElephant   = maximum [ v1+v2 | (ks1, v1) <- M.assocs santa
                                               , (ks2, v2) <- M.assocs santa
                                               , S.null $ S.intersection ks1 ks2
                                       ]
          print withElephant
