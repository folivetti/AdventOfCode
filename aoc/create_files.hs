#!/usr/bin/env stack
-- stack --resolver lts-19.31 script
module Main where

import System.Environment
import System.IO
import Control.Monad ( forM_ )

createFiles :: String -> IO ()
createFiles year = do
  forM_ [1 .. 25] $ \d -> do
      let day = if d < 10 then '0' : show d else show d
          fname = "src/Y" <> year <> "/Day" <> day <> ".hs"
          content = "module Y" <> year <> ".Day" <> day <> " ( solution ) where\n\nsolution :: IO ()\nsolution = undefined"
      writeFile fname content
  forM_ [1 .. 25] $ \d -> do
      let day = if d < 10 then '0' : show d else show d
      putStrLn $ "import qualified Y" <> year <> ".Day" <> day <> " as A" <> year <> day
  forM_ [1 .. 25] $ \d -> do
      let day = if d < 10 then '0' : show d else show d
      putStrLn $ "solveProblem " <> year <> " " <> day <> " = " <> "A" <> year <> day <> ".solution"
  forM_ [1 .. 25] $ \d -> do
      let day = if d < 10 then '0' : show d else show d
      putStr $ "Y" <> year <> ".Day" <> day <> ", "


main :: IO ()
main = do
    args <- getArgs
    case args of
      [year] -> createFiles year
      _ -> error "wrong format"
  
