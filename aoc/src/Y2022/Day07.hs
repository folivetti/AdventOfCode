{-# language LambdaCase #-}
{-# language TupleSections #-}
module Y2022.Day07 ( solution ) where

import Control.Monad.State
import qualified Data.Map as M
import Data.List ( tails )

parseCmd :: String -> State [String] [(String, Int)]
parseCmd cmd = case words cmd of
                 ["$", "cd", dname] -> const [] <$> cd dname
                 ["$", "ls"]        -> pure []
                 ["dir", _]         -> pure []
                 [size, _]          -> addFile (read size)
                 _                  -> error "invalid command"

cd :: String -> State [String] ()
cd ".." = modify tail
cd "/"  = put []
cd d    = modify (d:)

addFile :: Int -> State [String] [(String, Int)]
addFile s = gets $ map ((,s) . concatMap ('/' :)) . tails

getMinDir :: M.Map String Int -> Int
getMinDir fs = minimum . filter (>= required) $ M.elems fs
  where
      required = fs M.! "" + 30000000 - 70000000

solution :: IO ()
solution = do content <- lines <$> readFile "inputs/2022/input07.txt"
              let parsed = mapM parseCmd content
                  fs     = M.fromListWith (+) . concat $ evalState parsed []
              print $ M.foldr (+) 0 $ M.filter (<=100000) fs
              print $ getMinDir fs
