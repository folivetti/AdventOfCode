module AOC where

import qualified Y2015.Day01 as A201501
import qualified Y2015.Day02 as A201502
import qualified Y2018.Day01 as A201801

solveProblem :: Int -> Int -> IO ()
solveProblem 2015 1 = A201501.solution
solveProblem 2015 2 = A201502.solution
solveProblem 2018 1 = A201801.solution
solveProblem _ _ = error "not solved yet!"
