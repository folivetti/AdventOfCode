{-# language TupleSections #-}
module Y2015.Day21 (solution) where

import Control.Monad ( guard )

data Name = Santa | Boss deriving (Show, Eq)

data Player = Player
    { hitPoints :: Int
    , damage :: Int
    , armor :: Int
    , name :: Name
    } deriving Show

data EqType = Weapon | Armor | Ring deriving (Show, Eq)

data Equip = Equip
    { typ :: EqType
    , cost :: Int
    , atk :: Int
    , def :: Int
    } deriving (Show, Eq)


weapons :: [Equip]
weapons = [dagger, shortsword, warhammer, longsword, greataxe]
  where
    dagger     = Equip Weapon 8 4 0
    shortsword = Equip Weapon 10 5 0
    warhammer  = Equip Weapon 25 6 0
    longsword  = Equip Weapon 40 7 0
    greataxe   = Equip Weapon 74 8 0

armors :: [Equip]
armors = [leather, chainmail, splintmail, bandedmail, platemail]
  where 
    leather    = Equip Armor 13 0 1
    chainmail  = Equip Armor 31 0 2
    splintmail = Equip Armor 53 0 3
    bandedmail = Equip Armor 75 0 4
    platemail  = Equip Armor 102 0 5

rings :: [Equip]
rings = [dmg1, dmg2, dmg3, def1, def2, def3]
  where
    dmg1 = Equip Ring 25 1 0
    dmg2 = Equip Ring 50 2 0
    dmg3 = Equip Ring 100 3 0
    def1 = Equip Ring 20 0 1
    def2 = Equip Ring 40 0 2
    def3 = Equip Ring 80 0 3

fromEquips :: [Equip] -> Player
fromEquips xs = Player 100 totDmg totArmor Santa
  where
    totDmg   = sum $ map atk xs
    totArmor = sum $ map def xs

totCost :: [Equip] -> Int
totCost = sum . map cost

combinationsEquips :: [[Equip]]
combinationsEquips = do
  w <- weapons
  a <- armors
  r1 <- rings
  r2 <- rings
  guard $ r1 /= r2
  [[w], [w, a], [w, r1], [w,a,r1], [w,a,r1,r2]]


boss :: Player
boss   = Player 103 9 2 Boss

attacks :: Player -> Player -> Player
p1 `attacks` p2 = p2 { hitPoints = hitPoints p2 - dmg }
  where dmg = max 1 $ damage p1 - armor p2

isOver :: (Player, Player) -> Bool
isOver (p1, p2) = hitPoints p1 <= 0 || hitPoints p2 <= 0

santaWon :: (Player, Player) -> Bool
santaWon (p1, p2) = hitPoints santa > 0 where santa = if name p1 == Santa then p1 else p2

game :: (Player, Player) -> (Player, Player)
game (p1, p2)
  | isOver (p1, p2) = (p1, p2)
  | otherwise       = game (p1 `attacks` p2, p1)

solution :: IO ()
solution = do
  let part1 = minimum $ map totCost $ filter (santaWon . game . (,boss) . fromEquips) combinationsEquips
      part2 = maximum $ map totCost $ filter (not . santaWon . game . (,boss) . fromEquips) combinationsEquips
  print part1
  print part2
