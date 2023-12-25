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

step modules conjunctions = go ["broadcaster"] . toSecond (addPulse False)
  where
      toSecond f (a, b, c) = (a, f b, c)
      typeOf               = fmap fst . (modules Map.!?)

      send p xs state = hylo delayed coalg (xs, state)
        where 
            coalg ([], st) = Value st 
            coalg (y:ys, (st, c, rx)) = 
                let c'  = addPulse p c 
                    rx' = map (\(k, v) -> if y == k && not p then (k, v+1) else (k, v)) rx
                in case typeOf y of 
                  Just Broadcast -> Delayed (ys, (Map.insert y p st, c', rx'))
                  Just FlipFlop  -> Delayed (ys, (if p then st else Map.adjust not y st , c', rx'))
                  Just Conjunction -> Delayed (ys, (Map.insert y (not $ all (st Map.!) (conjunctions Map.! y)) st, c', rx'))
                  Nothing -> Delayed (ys, (st, c', rx'))
                

      go xs state = hylo delayed coalg (xs, state) 
        where 
          coalg ([], st) = Value st 
          coalg (x:xs, st@(state, count, _)) = Delayed (xs', st')
            where 
              to          = snd $ modules Map.! x
              pulse       = state Map.! x
              st'         = send pulse to st
              xs'         = xs <> filter (not.innactive) to
              innactive y = y `Map.notMember` modules || (typeOf y == Just FlipFlop && pulse)


solve modules = (part1, part2)
  where
    presses  = toList $ ana coalg st0 -- iterate step' st0
      where coalg st = ConsF st (step' st)

    part1 = hylo delayed coalg (1000, presses)
      where
        coalg (0, ((_,(a,b),_): _)) = Value (a*b)
        coalg (n, (_:xs)) = Delayed $ (n-1, xs)

    part2 = hylo delayed coalg (presses, [0,0,0,0], 0)
      where
        coalg ((_,_,rxs):xs, counts@[gc, sz, cm, xf], ix)
          | all (/=0) counts = Value (lcm gc (lcm sz (lcm cm xf)))
          | otherwise = Delayed $ (xs, zipWith f counts $ map (snd) rxs, ix+1)
          where f y x = if y == 0 && x > 0 then ix else y

    conjunctions = Map.fromListWith (<>) [(v, [k]) | (k, (_, vs)) <- Map.toList modules, v <- vs]
    st0          = (Map.fromList [(k, False) | (k, _) <- Map.toList modules], (0, 0), [("gc", 0),("sz",0),("cm",0),("xf",0)])
    step'        = step modules conjunctions

main :: IO ()
main = solve . Map.fromList . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input20.txt"
           >>= print
