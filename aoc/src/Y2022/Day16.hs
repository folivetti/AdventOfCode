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
step g t state = case openValve of
                   Nothing -> walk
                   Just o  -> o:walk
  where
      openValve | cur `S.member` open  = Nothing
                | flow == 0            = Nothing
                | otherwise            = Just $ state { _cost = _cost state + (t-1) * flow, _open = S.insert cur open }
         where 
             flow = fst (g M.! _cur state)
             cur = _cur state
             open = _open state
      walk = [ state {_cur = next} | next <- snd (g M.! _cur state)]

bfs :: Graph -> St -> Int -> M.Map (S.Set String) Int
bfs g st0 = go (uncurry M.singleton (fromState st0))
  where
      fromState st         = ((_cur st, _open st), _cost st)
      toState ((c, o), co) = St c co o
      go sts 0 = M.mapKeysWith max snd sts
      go sts t = go sts' (t-1)
        where sts' = M.fromListWith max $ concatMap (map fromState . step g t . toState) $ M.assocs sts

main :: IO ()
main = do content <- B.lines <$> B.readFile "inputs/2022/input16.txt"
          let g = M.fromList $ map (runParser parser) content
          print $ maximum $ bfs g (St "AA" 0 S.empty) 30
          let santa          = bfs g (St "AA" 0 S.empty) 26
              withElephant   = maximum [ v1+v2 | (ks1, v1) <- M.assocs santa
                                               , (ks2, v2) <- M.assocs santa
                                               , S.null $ S.intersection ks1 ks2
                                       ]
          print withElephant
