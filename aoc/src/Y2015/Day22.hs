{-# language TemplateHaskell #-}
module Y2015.Day22 (solution) where

import Control.Lens
import Data.Maybe ( isJust )

data Status = Status
  { _player :: Int,
    _boss :: Int,
    _mana :: Int,
    _shield :: Int,
    _poison :: Int,
    _recharge :: Int
  }
  deriving (Show, Eq)
makeLenses ''Status

data Magic = Missile | Drain | Shield | Poison | Recharge
    deriving (Show, Eq, Enum)

data Turn = Santa | Boss
    deriving (Show)

decEffects :: Status -> Status
decEffects = over shield dec0 . over poison dec0 . over recharge dec0
  where dec0 = max 0 . subtract 1

applyMagic :: Magic -> Status -> Status
applyMagic Missile  = (mana -~ 53) . (boss -~ 4)
applyMagic Drain    = (mana -~ 73) . (boss -~ 2) . (player +~ 2)
applyMagic Shield   = (mana -~ 113) . (set shield 6)
applyMagic Poison   = (mana -~ 173) . (set poison 6)
applyMagic Recharge = (mana -~ 229) . (set recharge 5)

manaOf :: Magic -> Int
manaOf Missile  = 53
manaOf Drain    = 73
manaOf Shield   = 113
manaOf Poison   = 173
manaOf Recharge = 229

isActive :: Getting Int Status Int -> Status -> Bool
isActive f = (>0) . view f

applyEffect :: Bool -> Turn -> Status -> Status
applyEffect b  turn = (if b then hitOne turn else id) . applyPoison . applyRecharge
  where
    applyPoison s   = if isActive poison s
                         then (boss -~ 3) s
                         else s
    applyRecharge s = if isActive recharge s
                         then (mana +~ 101) s
                         else s


santaLost, santaWon :: Status -> Bool
santaLost s = ((<=0) . view player) s || ((<=0) . view mana) s
santaWon  = (<=0) . view boss

hitOne :: Turn -> Status -> Status
hitOne Santa = player -~ 1
hitOne Boss  = id

data End = Win | Lose | Running deriving Show

outcome :: Status -> End
outcome s 
  | santaWon s = Win
  | santaLost s = Lose
  | otherwise = Running 

bestMana :: Bool -> Status -> Maybe Int
bestMana hard = go Santa (Just 0) Nothing
  where
    go turn curMana minMana s
      | santaWon s  = curMana
      | santaLost s = Nothing
      | isJust minMana && curMana > minMana = Nothing
      | otherwise   = let s'        = decEffects $ applyEffect hard turn s
                          hasShield = isActive shield s
                       in case outcome s' of
                           Win -> curMana
                           Lose -> Nothing
                           Running -> case turn of
                                        Boss -> go Santa curMana minMana $ if hasShield
                                                                     then (player -~ 2) s' 
                                                                     else (player -~ 9) s'
                                        Santa -> let budget = view mana s'
                                                     magics = filter (`available` s') 
                                                            $ filter ((<=budget) . manaOf)
                                                              [Missile, Drain, Shield, Poison, Recharge]
                                                  in foldr (getMin s') Nothing magics
      where
        nextMana magic = fmap (manaOf magic +) curMana

        getMin curSt magic Nothing = go Boss (nextMana magic) minMana $ applyMagic magic curSt
        getMin curSt magic mx
          | nextMana magic >= mx = mx
          | otherwise = case go Boss (nextMana magic) mx (applyMagic magic curSt) of
                          Nothing -> mx
                          my      -> min mx my


        available :: Magic -> Status -> Bool
        available magic st = case magic of
                               Shield -> view shield st == 0
                               Poison -> view poison st == 0
                               Recharge -> view recharge st == 0
                               _ -> True


solution :: IO ()
solution = do
    print $ bestMana False (Status 50 58 500 0 0 0)
    print $ bestMana True (Status 50 58 500 0 0 0)
