module Y2022.Day11 ( solution ) where

import Data.IntMap.Strict ( IntMap, (!) )
import qualified Data.IntMap.Strict as M
import Control.Monad.RWS 
import Data.List ( partition, sort )

data Monkey = Monkey { _worry   :: Integer -> Integer
                     , _cond    :: (Integer, Int, Int)
                     }

type Monkeys   = IntMap Monkey
newtype Counts = Counts { _uncount :: IntMap Integer } deriving Show
type Items     = IntMap [Integer]

instance Semigroup Counts where
  (Counts a) <> (Counts b) = Counts $ M.unionWith (+) a b
instance Monoid Counts where
  mempty = Counts M.empty

type Env = RWS Monkeys Counts Items

monkeys :: Monkeys
monkeys = M.fromList $ zip [0..]
                     [ Monkey (*7) (11, 5, 6)
                     , Monkey (*17) (19, 4, 2)
                     , Monkey (+2) (5, 7, 4)
                     , Monkey (+1) (2, 2, 1)
                     , Monkey (+6) (13, 7, 0)
                     , Monkey (^2) (7, 6, 3)
                     , Monkey (+3) (3, 1, 3)
                     , Monkey (+4) (17, 0, 5)
                     ]

divisors :: Integer
divisors = product [11, 19, 5, 2, 13, 7, 3, 17]

createCount :: M.Key -> Int -> Counts
createCount n = Counts . M.singleton n . fromIntegral

insert :: Int -> [Integer] -> Env ()
insert i m = do items <- gets (! i)
                modify $ M.insert i (items <> m)

turn :: Bool -> Int -> Env ()
turn b n = do items <- gets (! n)
              tell $ createCount n (length items)
              env <- asks (! n)
              let f = (if b then (`div` 3) else (`mod` divisors)) . _worry env
                  items' = map f items 
                  (d, m1, m2)  = _cond env
                  (toM1, toM2) = partition ((==0) . (`mod` d)) items'
              insert m1 toM1
              insert m2 toM2
              modify $ M.insert n []

items0 :: IntMap [Integer]
items0 = M.fromList $ zip [0..]
                     [ [97, 81, 57, 57, 91, 61]
                     , [88, 62, 68, 90]
                     , [74, 87]
                     , [53, 81, 60, 87, 90, 99, 75]
                     , [57]
                     , [54, 84, 91, 55, 59, 72, 75, 70]
                     , [95, 79, 79, 68, 78]
                     , [61, 97, 67]
                     ]

solution :: IO ()
solution = do let business    = replicateM 20 $ mapM_ (turn True) [0..7]
                  moreBusi    = replicateM 10000 $ mapM_ (turn False) [0..7]
                  (_, _, ws1) = runRWS business monkeys items0
                  (_, _, ws2) = runRWS moreBusi monkeys items0
                  solve       = print . product . take 2 . reverse . sort . M.elems . _uncount
              solve ws1
              solve ws2
