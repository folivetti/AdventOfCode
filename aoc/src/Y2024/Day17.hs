{-# LANGUAGE OverloadedStrings #-}
module Main where

import Utils
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Control.Monad.State.Strict
import Data.Bits
import Data.Maybe
import Data.SBV hiding (xor)
import qualified Data.SBV as SBV
import System.IO.Unsafe

data Machine = Machine { _A :: Int, _B :: Int, _C :: Int, _instructions :: [Int], _ix :: Int } deriving (Show, Eq, Ord)
type STMachine a = State Machine a

parser :: Parser Machine
parser = do string "Register A: "
            a <- decimal
            endOfLine
            string "Register B: "
            b <- decimal
            endOfLine
            string "Register C: "
            c <- decimal
            endOfLine >> endOfLine
            string "Program: "
            insts <- decimal `sepBy` (char ',')
            pure $ Machine a b c insts 0

combo :: Int -> STMachine Int
combo x
  | x <= 3 = pure x
  | x == 4 = gets _A
  | x == 5 = gets _B
  | x == 6 = gets _C
  | x == 7 = pure 0

to :: Int -> Int -> STMachine ()
to 0 x = do m <- get
            put (m{_A = x})
to 1 x = do m <- get
            put (m{_B = x})
to 2 x = do m <- get
            put (m{_C = x})

setPointer :: Int -> STMachine ()
setPointer ix = do m <- get
                   put (m{_ix=ix})

updatePointer :: STMachine ()
updatePointer = do ix <- gets _ix
                   m  <- get
                   put (m{_ix = ix+2})

--2,4,1,3,7,5,1,5,0,3,4,3,5,5,3,0
-- 2,4: B = A `mod` 8
-- 1,3: B = B xor 3
-- 7,5: C = A / 2^B
-- 1,5: B = B xor 5
-- 0,3: A = A / 2^3
-- 4,3: B = B xor C
-- 5,5: print (B `mod` 8)
-- 3,0: if A/=0 goto (2,4)
{-
go 0 _ _ = []
go A B C =
 let B = A `mod` 8
     B = xor B 3
     C = A `div` (2 ^ B)
     B = xor (xor B 5) C
  in (B `mod` 8) : go (A `div` 8) B C

B = A `mod` 8 xor 3
C = A / 2^ A `mod` 8 xor 3
B = B xor 5
A = A / 8
-}

run :: Int -> Int -> STMachine (Maybe Int)
run 0 op = do a <- gets _A
              c <- combo op
              let b = 2^c
              to 0 (a `div` b)
              updatePointer
              pure Nothing
run 1 op = do b <- gets _B
              to 1 (xor b op)
              updatePointer
              pure Nothing
run 2 op = do c <- combo op
              to 1 (c `mod` 8)
              updatePointer
              pure Nothing
run 3 op = do a <- gets _A
              if a==0
                then updatePointer
                else setPointer op
              pure Nothing
run 4 op = do b <- gets _B
              c <- gets _C
              to 1 (xor b c)
              updatePointer
              pure Nothing
run 5 op = do c <- combo op
              updatePointer
              pure $ Just (c `mod` 8)
run 6 op = do a <- gets _A
              c <- combo op
              let b = 2^c
              to 1 (a `div` b)
              updatePointer
              pure Nothing
run 7 op = do a <- gets _A
              c <- combo op
              let b = 2^c
              to 2 (a `div` b)
              updatePointer
              pure Nothing

getInstruction :: STMachine (Int, Int)
getInstruction = do ix <- gets _ix
                    i  <- gets ((!! ix) . _instructions)
                    op <- gets ((!! (ix+1)) . _instructions)
                    pure (i, op)

step :: STMachine (Maybe Int)
step = do (i, op) <- getInstruction
          run i op
end :: STMachine Bool
end = do ix <- gets _ix
         n  <- gets (length . _instructions)
         pure (ix>=n)

runMachine mac = evalState go mac
  where
    go = do finished <- end
            if finished
               then pure []
               else do mx <- step
                       mxs <- go
                       pure (mx:mxs)

solve1 mac = prog (_A mac) (_A mac `mod` 8) (_C mac) -- catMaybes $ runMachine mac

solve2 mac = unsafePerformIO $ do
  res <- optLexicographic $
      do a  <- free "a" :: Symbolic SWord64
         minimize "smallest" a
         sbvProg a (_instructions mac)
  case getModelValue "a" res of
      Just x  -> pure (x :: Word64)
      Nothing -> fail "no solution"

prog 0 _ _ = []
prog a b c =
 let b'' = xor b 3
     c' = a `div` (2 ^ b'')
     b' = xor (xor b'' 5) c'
 in (b' `mod` 8) : prog (a `div` 8) ((a `div` 8) `mod` 8) c'

sbvProg :: SWord64 -> [Int] -> Symbolic ()
sbvProg a [] = constrain (a .== 0)
sbvProg a (x:xs) = do
  constrain (a ./= 0)
  b <- pure (a .&. 7)
  b <- pure (SBV.xor b 3)
  c <- pure (sShiftRight a b) --`div` (2 ^ b))
  b <- pure $ SBV.xor (SBV.xor b 5) c
  constrain ((b .&. 7) .== fromIntegral x) -- b `mod` 8
  a <- pure (sShiftRight a (3 :: SWord64))
  sbvProg a xs

f &&& g = \x -> (f x, g x)

solve' = solve1 &&& solve2

main :: IO ()
main = solve' . runParser parser <$> B.readFile "inputs/2024/input17.txt"
         >>= print
