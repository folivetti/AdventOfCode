{-# language OverloadedStrings #-}

module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace ( traceShow ) 
import Control.Arrow ( (&&&) )
import Data.Bifunctor ( second )

data Module = FlipFlop | Conjunction | Broadcast deriving (Show, Eq) 

parser = parseFlipFlop <|> parseConjunction <|> parseBroadcast
   where 
     parseFlipFlop = do char '%' 
                        name <- many' letter_ascii 
                        string " -> "
                        to <- many' letter_ascii `sepBy` string ", "
                        pure (name, (FlipFlop, to))
     parseConjunction = do char '&' 
                           name <- many' letter_ascii 
                           string " -> "
                           to <- many' letter_ascii `sepBy` string ", "
                           pure (name, (Conjunction, to))
     parseBroadcast = do string "broadcaster"
                         string " -> "
                         to <- many' letter_ascii `sepBy` string ", "
                         pure ("broadcaster", (Broadcast, to))

addPulse False (x, y) = (x+1, y)
addPulse True  (x, y) = (x, y+1)

step modules conjunctions k = go ["broadcaster"] . toSecond (addPulse False)
  where
      toSecond f (a, b, c) = (a, f b, c)
      typeOf = fmap fst . (modules Map.!?)

      send p [] state     = state
      send p (x:xs) (state, count, rx) = case typeOf x of 
                              Just Broadcast -> send p xs $ (Map.insert x p state, addPulse p count, rx')
                              Just FlipFlop  -> send p xs $ if p then (state, addPulse p count, rx') else (Map.adjust not x state, addPulse p count, rx')
                              Just Conjunction -> send p xs $ if all (state Map.!) (conjunctions Map.! x)
                                                               then (Map.insert x False state, addPulse p count, rx') 
                                                               else (Map.insert x True state, addPulse p count, rx')
                              Nothing -> send p xs (state, addPulse p count, rx')
            where 
              rx' | x == k    = if p then rx else (rx+1)
                  | otherwise = rx

      go []     state = state
      go (x:xs) st@(state, count, rx) = go xs' state'
        where 
          to          = snd $ modules Map.! x
          pulse       = state Map.! x
          state'      = send pulse to st
          xs'         = xs <> filter (not.innactive) to
          innactive y = y `Map.notMember` modules || (typeOf y == Just FlipFlop && pulse)



solve modules = (part1, part2)
  where 
    presses = iterate (step' "rx") st0
    eachP   = map (\k -> iterate (step' k) st0) ["gc", "sz", "cm", "xf"]
    part1 = calcFinal $ map getPulses $ Prelude.take 1001 $ presses
    part2 = foldr lcm 1 $ map (findRx 0) eachP
    conjunctions = Map.fromListWith (<>) [(v, [k]) | (k, (_, vs)) <- Map.toList modules, v <- vs]
    st0          = (Map.fromList [(k, False) | (k, _) <- Map.toList modules], (0, 0), 0) -- <<< here rx
    step'        = step modules conjunctions

    calcFinal (x:xs) = let (a, b)   = last xs
                        in a * b

    getPulses (_, p, _) = p 
    getSt (s, _, _) = s 
    getRx (_, _, rx) = rx 

    findRx ix [] = 0
    findRx ix (x:xs) 
      | getRx x > 0 = ix 
      | otherwise   = findRx (ix+1) xs

main :: IO ()
main = do 
         solve . Map.fromList . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input20.txt"
           >>= print

