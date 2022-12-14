module Main where

import Data.List.Split ( chunksOf )

runInstruction :: [[String]] -> Int -> [Int]
runInstruction [] _               = []
runInstruction (["noop"]:is) x    = x : runInstruction is x
runInstruction (["addx", y]:is) x = x : x : runInstruction is x' where x' = x + read y
runInstruction _ _                = error "no parse for this instruction"

main :: IO ()
main = do content <- map words . lines <$> readFile "inputs/2022/input10.txt" 
          let trace    = 1 : runInstruction content 1
              signal   = zipWith (*) trace [0..]
              draw x y = if abs (x-y) <= 1 then '#' else '.'
              crt      = map (zipWith draw [0..]) $ chunksOf 40 $ tail trace
          print $ sum $ map (signal !!) [20, 60, 100, 140, 180, 220]
          mapM_ print crt
