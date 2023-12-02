{-# language BinaryLiterals #-}
{-# language TupleSections #-}
module Main where

import Data.Bits
import Data.Word
import Control.Monad.State
import Data.List ( splitAt, sort )
import Data.Bifunctor
import qualified Data.Map as M

type Game a = State (String, [Word8]) a

blocks :: [[Word8]]
blocks = [ [0b0011110]
         , [0b0001000
           ,0b0011100
           ,0b0001000]
         , [0b0000100
           ,0b0000100
           ,0b0011100]
         , [0b0010000
           ,0b0010000
           ,0b0010000
           ,0b0010000]
         , [0b0011000
           ,0b0011000]
         ]

right :: [Word8] -> [Word8]
right block
  | any (`testBit` 0) block = block
  | otherwise               = map (`shiftR` 1) block
left :: [Word8] -> [Word8]
left block
  | any (`testBit` 6) block = block
  | otherwise               = map (`shiftL` 1) block

move :: Char -> [Word8] -> [Word8]
move '>' = right
move '<' = left
move _   = id

s0 :: Word8
s0 = 0b1111111

fill :: Int -> [Word8] -> [Word8]
fill n = (replicate (n+3) 0 <>)

nextJet :: Game Char
nextJet = do c <- gets (head . fst)
             modify (first tail)
             pure c
                           
hasCollision :: [Word8] -> [Word8] -> Bool
hasCollision block = any (/=0) . zipWith (.&.) block

isSettled :: [Word8] -> Game Bool
isSettled block = gets (hasCollision block . tail . snd)
             
dropBlock :: [Word8] -> Game ()
dropBlock block = do modify $ second (fill $ length block)
                     loopBlock block []
                     modify (second (dropWhile (==0)))
                     pure ()

moveJet :: [Word8] -> Game [Word8]
moveJet block = do block' <- (`move` block) <$> nextJet
                   collision <- gets (hasCollision block' . snd)
                   pure $ if collision then block else block'

loopBlock :: [Word8] -> [Word8] -> Game ()
loopBlock block xs = do block' <- moveJet block
                        settled <- isSettled block'
                        if settled 
                         then modify $ second ((reverse xs <>) . zipWith (.|.) (block' <> repeat 0))
                         else do stck <- gets snd
                                 modify (second tail)
                                 loopBlock block' (head stck : xs)

ex :: String
ex = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

getHeight :: Game Int
getHeight = gets $ length . init . snd

solve :: Int -> String -> Int
solve n instrs = let game = go (cycle blocks) M.empty n
                  in evalState game (cycle instrs, [s0]) 
  where
     go _ _ 0         = getHeight
     go (b:bs) seen x = do
       dropBlock b
       key <- gets ((b,) . bimap (take 50) (take 50))
       val <- (n-x+1,) <$> getHeight
       case seen M.!? key of
         Nothing     -> go bs (M.insert key val seen) (x-1)
         Just (y, h) -> do hh <- getHeight
                           let rev    = sort $ M.elems seen
                               howMany = n - x + 1 - y
                               loopH   = hh - h
                               (rep, lft) = (x - 1) `divMod` howMany
                           pure $ hh + rep * loopH + snd (rev !! (lft + y - 1)) - snd (rev !! (y - 1))

main :: IO ()
main = do content <- init <$> readFile "inputs/2022/input17.txt"
          print $ solve 2022 content
          print $ solve  1000000000000 content
