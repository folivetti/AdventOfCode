module Y2015.Day22 (solution) where

import Data.Bifunctor ( bimap, first, second )
import Data.Maybe ( isJust )

data Magic = Missile | Drain | Shield | Poison | Recharge deriving (Show, Eq)

data Player = Player
    { hitPoints :: Int
    , mana :: Int
    , shield :: Int
    , poison :: Int
    , recharge :: Int
    } deriving Show

addMana :: Int -> Player -> Player
addMana x p        = p { mana = mana p + x }
addHit :: Int -> Player -> Player
addHit x p         = p { hitPoints = hitPoints p + x }
activateShield :: Player -> Player
activateShield p   = p { shield = 6 }
activatePoison :: Player -> Player
activatePoison p   = p { poison = 6 }
activateRecharge :: Player -> Player
activateRecharge p = p { recharge = 5 }
decShield :: Player -> Player
decShield p        = p { shield = dec0 (shield p) }
decPoison :: Player -> Player
decPoison p        = p { poison = dec0 (poison p) }
decRecharge :: Player -> Player
decRecharge p      = p { recharge = dec0 (recharge p) }
decEffects :: Player -> Player
decEffects         = decShield.decPoison.decRecharge
dec0 :: Int -> Int
dec0               = max 0 . subtract 1

applyMagic :: Magic -> (Player, Int) -> (Player, Int)
applyMagic Missile s  = bimap (addMana (-53)) (subtract 4) s
applyMagic Drain s    = bimap (addHit 2 . addMana (-73)) (subtract 2) s
applyMagic Shield s   = first (activateShield . addMana (-113)) s
applyMagic Poison s   = first (activatePoison . addMana (-173)) s
applyMagic Recharge s = first (activateRecharge . addMana (-229)) s

manaOf :: Magic -> Int
manaOf Missile = 53
manaOf Drain = 73
manaOf Shield = 113
manaOf Poison = 173
manaOf Recharge = 229

applyEffect :: (Player, Int) -> (Player, Int)
applyEffect = applyPoison . applyRecharge
  where
    applyPoison   s = if isActive poison s   then second (subtract 3) s else s
    applyRecharge s = if isActive recharge s then first (addMana 101) s else s
    isActive f      = (>0) . f . fst

santa :: Magic -> (Player, Int) -> (Player, Int)
santa m = applyMagic m . first decEffects . applyEffect

boss :: (Player, Int) -> (Player, Int)
boss = applyShield . first decEffects . applyEffect
  where applyShield s = if (shield.fst) s > 0
                          then first (addHit (-2)) s
                          else first (addHit (-9)) s


santaLost :: (Player, Int) -> Bool
santaLost (p, b) = hitPoints p <= 0 || mana p < 0

santaWon :: (Player, Int) -> Bool
santaWon (p, b) = hitPoints p > 0 && b == 0

hitOne :: (Player, Int) -> (Player, Int)
hitOne = first (addHit (-1))

bestMana :: (Player, Int) -> Maybe Int
bestMana st = go st True 0
  where
    go s@(p,_) b m
      | santaWon s  = Just m
      | santaLost s = Nothing
      | null magics = Nothing
      | otherwise = if b
                      then foldr getMin Nothing magics
                      else go (boss s) True m
      where
        step m' = go (santa m' s) False (m + manaOf m')
        getMin m' Nothing  = step m'
        getMin m' (Just n) = if m + manaOf m' > n
                               then Just n
                               else case step m' of
                                      Nothing -> Just n
                                      Just n' -> Just (min n n')
        magics  = filter (noMana &&& noShield &&& noPoison &&& noRecharge) [Missile, Drain, Shield, Poison, Recharge]
        noMana m'           = manaOf m' <= mana p + if recharge p > 0 then 101 else 0
        noShield Shield     = shield p <= 1
        noShield _          = True
        noPoison Poison     = poison p <= 1
        noPoison _          = True
        noRecharge Recharge = recharge p <= 1
        noRecharge _        = True
        f &&& g             = \x -> f x && g x

solution :: IO ()
solution = do
    print $ bestMana (Player 50 500 0 0 0, 58)
