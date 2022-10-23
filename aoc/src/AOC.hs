module AOC where

import qualified Y2015.Day01 as A201501
import qualified Y2015.Day02 as A201502
import qualified Y2015.Day03 as A201503
import qualified Y2015.Day04 as A201504
import qualified Y2015.Day05 as A201505
import qualified Y2015.Day06 as A201506

import qualified Y2018.Day01 as A201801

solveProblem :: Int -> Int -> IO ()
solveProblem 2015 1 = A201501.solution
solveProblem 2015 2 = A201502.solution
solveProblem 2015 3 = A201503.solution
solveProblem 2015 4 = A201504.solution
solveProblem 2015 5 = A201505.solution
solveProblem 2015 6 = A201506.solution
solveProblem 2015 7 = A201502.solution
solveProblem 2015 8 = A201502.solution
solveProblem 2015 9 = A201502.solution
solveProblem 2015 10 = A201502.solution
solveProblem 2015 11 = A201502.solution
solveProblem 2015 12 = A201502.solution
solveProblem 2015 13 = A201502.solution
solveProblem 2015 14 = A201502.solution
solveProblem 2015 15 = A201502.solution
solveProblem 2015 16 = A201502.solution
solveProblem 2015 17 = A201502.solution
solveProblem 2015 18 = A201502.solution
solveProblem 2015 19 = A201502.solution
solveProblem 2015 20 = A201502.solution
solveProblem 2015 21 = A201502.solution
solveProblem 2015 22 = A201502.solution
solveProblem 2015 23 = A201502.solution
solveProblem 2015 24 = A201502.solution
solveProblem 2015 25 = A201502.solution

solveProblem 2018 1 = A201801.solution

solveProblem _ _ = error "not solved yet!"
