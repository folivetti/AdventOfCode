{-# language OverloadedLists #-}
module Y2016.Day11 ( solution ) where

import Data.IntMap.Strict ( IntMap, (!) )
import qualified Data.Set as S
import qualified Data.IntMap.Strict as M
import Data.Bifunctor ( first, second, bimap )
import Algorithm.Search ( aStar )
import Data.List ( nubBy )

-- data Element   = H | L deriving (Show, Eq, Ord)
data Element   = P | Co | Cu | Ru | Pl | E | D deriving (Show, Eq, Ord)
data Component = Microchip Element | Generator Element deriving (Show, Eq, Ord)
type Floor     = S.Set Component

data Stat = Stat { _floors :: IntMap Floor
                 , _elevator :: Int
                 } deriving (Show, Eq, Ord)

isGoal :: Int -> Stat -> Bool
isGoal n = (==n) . length . (! 3) . _floors

isChip :: Component -> Bool
isChip (Microchip _) = True
isChip _ = False
isGen :: Component -> Bool
isGen (Generator _) = True
isGen _ = False

neighbors :: Stat -> [Stat]
neighbors (Stat fs e) = concatMap moves [e' | e' <- [e-1, e+1], isValidElevator e']
  where
      onemove ([], _)    = []
      onemove (x:xs, ys) = (xs, x:ys) : map (first (x:)) (onemove (xs, ys))

      twomoves ([], _)   = []
      twomoves ([_], _)  = []
      twomoves (x:xs, ys) = map (second (x:)) (onemove (xs, ys)) <> map (first (x:)) (twomoves (xs, ys))

      allmoves True xys
        | null twos = ones
        | otherwise = twos
        where xys' = bimap S.toList S.toList xys
              ones = filter isBothFloorsValid $ onemove xys'
              twos = filter isBothFloorsValid $ twomoves xys'
      allmoves False xys 
        | null ones = twos
        | otherwise = ones
        where xys' = bimap S.toList S.toList xys
              ones = filter isBothFloorsValid $ onemove xys'
              twos = filter isBothFloorsValid $ twomoves xys'

      applyMoves e' (curFloor, nxtFloor) = let fs' = M.insert e' (S.fromList nxtFloor) 
                                                   $ M.insert e (S.fromList curFloor) fs
                                            in Stat fs' e'

      moves e' = map (applyMoves e')
               $ nubBy (\x y -> myEq (snd x) (snd y))
               $ filter isBothFloorsValid
               $ allmoves (e' > e) (fs ! e, fs ! e')

myEq xs ys
  | xchips == ychips && xgens == ygens = True
  | otherwise = xs==ys
  where 
      xchips = length $ filter isChip xs
      ychips = length $ filter isChip ys
      xgens  = length $ filter isGen xs
      ygens  = length $ filter isGen ys

isBothFloorsValid :: ([Component], [Component]) -> Bool
isBothFloorsValid (x, y) = isValidFloor x && isValidFloor y

isValidFloor :: [Component] -> Bool
isValidFloor components
  | null gens = True
  | otherwise = all ((`elem` gens) . toGen) chips
  where
      chips = filter isChip components
      gens  = filter isGen components
      toGen (Microchip x) = Generator x
      toGen x = x

isValidElevator :: Int -> Bool
isValidElevator x = x >= 0 && x <= 3

cost :: Stat -> Stat -> Int
cost _ = const 1

heur :: Stat -> Int
heur (Stat fs _) = 3 * length (fs ! 0) + 2 * length (fs ! 1) + length (fs ! 2)

s0, s1 :: Stat
s0 = Stat (M.fromList [(0, [Microchip P, Generator P]), (1, [Generator Co, Generator Cu, Generator Ru, Generator Pl]), (2, [Microchip Co, Microchip Cu, Microchip Ru, Microchip Pl]), (3, []) ]) 0
s1 = Stat (M.fromList [(0, [Microchip P, Generator P, Microchip E, Generator E, Microchip D, Generator D]), (1, [Generator Co, Generator Cu, Generator Ru, Generator Pl]), (2, [Microchip Co, Microchip Cu, Microchip Ru, Microchip Pl]), (3, []) ]) 0

fromJust (Just x) = x

solution :: IO ()
solution = do
   print $ fst <$> aStar neighbors cost heur (isGoal 10) s0
   print $ fst <$> aStar neighbors cost heur (isGoal 14) s1
