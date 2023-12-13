{-# language DeriveFunctor #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.List ( intercalate )
import Control.Arrow ( (&&&) )
import Data.Array ( listArray, (!), bounds )

parser = do spring <- many' (char '.' <|> char '#' <|> char '?')
            space
            nums <- decimal `sepBy` char ','
            pure (spring, nums)

data Springs a e = DeadEnd | End | Only a e | Branch e (a, a) e
    deriving Functor

-- dynamic programming
fillUp (springs, vals) = dyna alg coalg (rngVals, rngSprings)
  where
    rngSprings = [0 .. nSprings - 1]
    rngVals    = [0 .. nVals - 1]
    nSprings   = length springs 
    nVals      = length vals
    aSprings   = listArray (0, nSprings - 1) springs 
    aVals      = listArray (0, nVals - 1) vals 

    takeArr ix n arr = [arr ! i | i <- [ix .. ix + n - 1], i <= snd (bounds arr)]
    getArr ix arr
      | ix >= lo && ix <= hi = Just (arr ! ix) 
      | otherwise            = Nothing 
      where (lo, hi) = bounds arr

    coalg ([], []) = NilF 
    coalg ([], _) = ConsF (nVals, nSprings) ([], [])
    coalg (ixs1, ixs2) = if null ixs2 then ConsF (head ixs1, nSprings) (tail ixs1, rngSprings)
                                      else ConsF (head ixs1, head ixs2) (ixs1, tail ixs2)

    alg NilF = 0
    alg (ConsF (ix, iy) table)
      | iy >= nSprings = 0
      | ix >= nVals    = 0
      | otherwise      = case aSprings ! iy of 
                           '.' -> dot 
                           '#' -> hash 
                           '?' -> dot + hash
                           _   -> error "invalid char"
      where 
        offset m n = m * (nSprings + 1) + n - 1
        canBlock   = (`elem` ("?#" :: String))
        noBlocks   = all (/='#')

        dot        = index table (offset 0 1)

        hash       = let n = aVals ! ix 
                         s = takeArr iy n aSprings 
                      in if length s == n && all canBlock s
                          then case getArr (iy + n) aSprings of
                                 Nothing  -> if ix + 1 == nVals then 1 else 0
                                 Just '#' -> 0
                                 Just _   -> if ix + 1 == nVals 
                                               then if noBlocks (takeArr (iy+n+1) nSprings aSprings) then 1 else 0
                                               else index table (offset 1 (n+1))
                          else 0


-- Brute force if you want
fillUp' = hylo alg coalg 
  where
    canBlock     = (`elem` ("?#" :: String))
    canFill xs n 
      | length xs >  n = allCan &&  (xs !! n) `elem` ("?." :: String)
      | length xs == n = allCan
      | otherwise      = False
      where 
        allCan = all canBlock (Prelude.take n xs) 

    fill n xs
      | length xs == n = replicate n '#'
      | otherwise      = replicate n '#' <> "."

    alg End                 = 1 -- [""]
    alg DeadEnd             = 0 -- []
    alg (Only x xs)         = xs -- map (x <>) xs
    alg (Branch l (x, y) r) = l + r -- map (x <>) l <> map (y <>) r

    coalg ("", []) = End -- everything was consumed
    coalg (xs, []) = if all (`elem` (".?" :: String)) xs 
                        then Only (replicate (length xs) '.') ("", []) -- we inserted all the springs, fill up with '.'
                        else DeadEnd -- there's still an unfilled block
    coalg ("", ns) = DeadEnd -- end of input but there are springs left
    coalg ('.':xs, ns) = let as = Prelude.takeWhile (=='.') xs 
                             bs = dropWhile (=='.') xs 
                          in Only ('.':as) (bs, ns) -- a '.' cannot have a spring, skip
    coalg ('#':xs, n:ns) = if canFill xs (n-1) -- check if we can fill this part with n springs
                              then Only (fill n ('#':xs)) (drop n xs, ns)
                              else DeadEnd -- if we can't, it is a dead end 
    coalg ('?':xs, (n:ns)) = if canFill xs (n-1) -- check if we can fill this part with n springs 
                                then Branch (xs, n:ns) (".", fill n ('#':xs)) (drop n xs, ns)
                                else Only "." (xs, n:ns) -- if we can't, we won't
    coalg (_, _) = error "unknown char in input"

unfoldSprings (xs, ys) = (intercalate "?" $ replicate 5 xs, concat $ replicate 5 ys)

solve1 = sum . map fillUp 
solve2 = sum . map (fillUp . unfoldSprings)

solve = (solve1 &&& solve2)

main :: IO ()
main = solve . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input12.txt"
         >>= print
