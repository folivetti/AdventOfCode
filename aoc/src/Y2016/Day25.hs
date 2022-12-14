module Main where

myFun :: Int -> [Int]
myFun a = toBin $ a + 633*4
  where
    toBin 0 = []
    toBin x = let (q ,r) = x `quotRem` 2
               in r : toBin q

search :: Int
search = go 0
  where
    go n
      | b == take l (cycle [0,1]) = n
      | otherwise                 = go (n+1)
      where
         b = myFun n
         l = length b

main :: IO ()
main = print search
