module Main (main) where

import System.Environment ( getArgs )
import Text.Read ( readMaybe )
import AOC ( solveProblem )

main :: IO ()
main = do
    args <- getArgs
    case args of
      (sy:sd:_) -> case solveProblem <$> readMaybe sy <*> readMaybe sd of 
                     Nothing -> error "stack run year day"
                     Just f  -> putStrLn ("Solving " <> show sy <> " " <> show sd) >> f
      _         -> error "stack run year day"
