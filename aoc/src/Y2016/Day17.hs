module Y2016.Day17 ( solution ) where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Set as S
import Data.List ( find )
import Control.Monad( guard )

data Dir = Up | Down | Lft | Rgt deriving (Eq, Enum)
data Path = Path { _path :: String, _cost :: Int, _coord :: (Int, Int) } deriving (Show, Eq)

instance Show Dir where
  show Up = "U"
  show Down = "D"
  show Lft = "L"
  show Rgt = "R"

hash :: String -> String
hash = unpack . encode . MD5.hash . pack
{-# inline hash #-}

hashFromPath :: String -> String
hashFromPath = hash . (passcode <>)
{-# inline hashFromPath #-}

isOpen :: Dir -> String -> Bool
isOpen Up        (x:xs) = x `elem` "bcdef"
isOpen Down    (_:x:xs) = x `elem` "bcdef"
isOpen Lft   (_:_:x:xs) = x `elem` "bcdef"
isOpen Rgt (_:_:_:x:xs) = x `elem` "bcdef"
isOpen _ xs = error xs

move :: Dir -> (Int, Int) -> (Int, Int)
move Up (x, y)   = (x-1, y)
move Down (x, y) = (x+1, y)
move Lft (x, y)  = (x, y-1)
move Rgt (x, y)  = (x, y+1)

withinBounds :: (Int, Int) -> Bool
withinBounds (x, y) = x >= 0 && x <= 3 && y >= 0 && y <= 3

nextStates :: Path -> [Path]
nextStates (Path path cost xy) = do
  guard $ xy /= goal
  m <- [Up .. Rgt]
  let xy' = move m xy
  guard $ withinBounds xy'
  let p = path <> show m
  guard $ isOpen m (hashFromPath path)
  pure $ Path p (cost + 1) xy'

bfs :: Path -> [Path]
bfs s = go S.empty [s] []
  where
    go _    [] [] = []
    go hist [] ys = go hist (reverse ys) []
    go hist (x:xs) ys
      | coord `S.member` hist = go hist xs ys
      | otherwise             = x : go (S.insert coord hist) xs (nextStates x <> ys)
      where coord = _path x <> show (_coord x)

s0, goal :: (Int, Int)
s0   = (0, 0)
goal = (3, 3)

passcode :: String
--passcode = "njfxhljp"
passcode = "dmypynyp"

solution :: IO ()
solution = do let steps = bfs $ Path "" 0 s0
              print $ find ((==goal) . _coord) steps
              print $ last $ filter ((==goal) . _coord) steps
