{-# language OverloadedStrings #-}
module Y2015.Day19 (solution) where

import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List ( nub )
import qualified Data.Set as S

parseRules :: [Text] -> [(Text, Text)]
parseRules = map parse
  where
      parse xs = let rl = T.splitOn " => " xs in (rl !! 0, rl !! 1)

replacements :: [(Text, Text)] -> Text -> [Text]
replacements rules codex = nub $ concatMap (applyRule codex) rules
  where
    applyRule :: Text -> (Text, Text) -> [Text]
    applyRule xss (a, b) =  map replace $ T.breakOnAll a xss
      where
        replace (x, "") = x
        replace (x, y)  = x <> b <> T.drop n y
        n = T.length a

findCodex codex rules start = go [start] 0 S.empty
  where
      go xs it tabu
        | codex `elem` xs = it
        | otherwise       = go xs' (it + 1) tabu'
        where
          xs'   = nub $ filter allowed $ concatMap (replacements rules) xs
          tabu' = S.union tabu (S.fromList xs')
          allowed x = x `notElem` tabu && T.length x <= T.length codex

--codex = "HOHOHO"
--rules = [("H", "HO"), ("H", "OH"), ("O", "HH"), ("e", "H"), ("e", "O")]

solution :: IO ()
solution = do
  content <- T.lines <$> TIO.readFile "inputs/2015/input19.txt"
  let codex = last content
      rules = parseRules $ (init . init) content
      gens = findCodex codex rules "e"
  print $ length $ replacements rules codex
  print gens
