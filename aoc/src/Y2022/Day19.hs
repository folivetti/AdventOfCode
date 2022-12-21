{-# language OverloadedStrings #-}
module Main where

import Data.Maybe ( catMaybes )
import Utils ( runParser )
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

data Minerals = Minerals { _ore :: Int
                         , _clay :: Int
                         , _obsidian :: Int
                         , _geode :: Int
                         } deriving Show

data Blueprints = Blueprints { _oreB :: Minerals
                             , _clayB :: Minerals
                             , _obsidianB :: Minerals
                             , _geodeB :: Minerals
                             } deriving Show

add, sub :: Minerals -> Minerals -> Minerals
add (Minerals a b c d) (Minerals x y z w) = Minerals (a+x) (b+y) (c+z) (d+w)
sub (Minerals a b c d) (Minerals x y z w) = Minerals (a-x) (b-y) (c-z) (d-w)

canBuild :: Minerals -> Minerals -> Bool
canBuild (Minerals a b c d) (Minerals x y z w) =
    and [ cmp a x, cmp b y, cmp c z, cmp d w ]
    where
        cmp _ 0 = True
        cmp l r = r <= l

searchBlueprint :: Blueprints -> Int -> Int
searchBlueprint bps = go (Minerals 0 0 0 0, Minerals 1 0 0 0) (False, False, False) 0
  where
    buildOre, buildClay, buildObsidian, buildGeode :: (Minerals, Minerals) -> (Minerals, Minerals)
    buildOre (mins, bots)      = (sub mins (_oreB bps), bots{ _ore = _ore bots + 1 })
    buildClay (mins, bots)     = (sub mins (_clayB bps), bots{ _clay = _clay bots + 1 })
    buildObsidian (mins, bots) = (sub mins (_obsidianB bps), bots{ _obsidian = _obsidian bots + 1 })
    buildGeode (mins, bots)    = (sub mins (_geodeB bps), bots{ _geode = _geode bots + 1 })

    go :: (Minerals, Minerals) -> (Bool, Bool, Bool) -> Int -> Int -> Int
    go (mins, _)    _ best 0 = max best $ _geode mins
    go (mins, bots) (c1, c2, c3) best t
      | maxGeodes <= best           = best
      | canBuild mins (_geodeB bps) = go (buildGeode (mins', bots)) (False, False, False) best (t-1)
      | otherwise                   = maximum nexts
      where
        pOre      = (`canBuild` _oreB bps)
        pClay     = (`canBuild` _clayB bps)
        pObsidian = (`canBuild` _obsidianB bps)

        plans      = [_oreB bps, _clayB bps, _obsidianB bps, _geodeB bps]
        enough f x = f x >= maximum (map f plans)

        funs  = [ (buildObsidian, pObsidian, not . enough _obsidian, not c1)
                , (buildClay, pClay, not . enough _clay, not c2)
                , (buildOre, pOre, not . enough _ore, not c3)
                ]
        nexts = [ go next (False, False, False) best (t-1) | (f, p, b, c) <- funs
                                     , p mins && b bots && c
                                     , let next = f (mins', bots)
                ] <> [go (mins', bots) (pOre mins, pClay mins, pObsidian mins) best (t-1)]

        mins' = add mins bots
        maxGeodes = t * (t - 1) `div` 2 + t * _geode bots + _geode mins

ex1, ex2 :: Blueprints
ex1 = Blueprints (Minerals 4 0 0 0) (Minerals 2 0 0 0) (Minerals 3 14 0 0) (Minerals 2 0 7 0)
ex2 = Blueprints (Minerals 2 0 0 0) (Minerals 3 0 0 0) (Minerals 3 8 0 0) (Minerals 3 0 12 0)

parser :: Parser Blueprints
parser = do string "Blueprint "
            decimal
            string ": Each ore robot costs "
            x <- decimal
            let ore = Minerals x 0 0 0
            string " ore. Each clay robot costs "
            y <- decimal
            let clay = Minerals y 0 0 0
            string " ore. Each obsidian robot costs "
            a <- decimal
            string " ore and "
            b <- decimal
            let obsidian = Minerals a b 0 0
            string " clay. Each geode robot costs "
            c <- decimal
            string " ore and "
            d <- decimal
            let geode = Minerals c 0 d 0
            string " obsidian."
            pure $ Blueprints ore clay obsidian geode

main :: IO ()
main = do content <- map (runParser parser) . B.lines <$> B.readFile "inputs/2022/input19.txt"
          let part1 = map (`searchBlueprint` 24) content
              part2 = map (`searchBlueprint` 32) $ Prelude.take 3 content
          print $ sum $ zipWith (*) [1..] part1
          print $ product part2
