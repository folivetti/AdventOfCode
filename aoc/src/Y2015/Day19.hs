{-# language OverloadedStrings #-}
module Y2015.Day19 (solution) where

import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List ( nub, sortOn )
import Data.Maybe ( isJust )

parseRules :: [Text] -> [(Text, Text)]
parseRules = map parse
  where
      parse xs = let rl = T.splitOn " => " xs in (rl !! 0, rl !! 1)

replacements :: [(Text, Text)] -> Text -> [Text]
replacements rules codex = nub $ concatMap (applyRule codex) rules

applyRule :: Text -> (Text, Text) -> [Text]
applyRule xss (a, b) =  map replace $ T.breakOnAll a xss
  where
    replace (x, "") = x
    replace (x, y)  = x <> b <> T.drop n y
    n = T.length a

findCodex :: Text -> [(Text, Text)] -> Text -> Maybe Int
findCodex codex rules target = go 0 codex
  where
      go :: Int -> Text -> Maybe Int
      go it xs
        | xs == target    = Just it
        | null candidates = Nothing
        | otherwise       = head $ concatMap (mapFilter . apply) candidates
        where
          candidates      = sortLen $ filterSub rules
          isSubstringOf x = (/="") . snd . T.breakOn x
          apply           = applyRule xs . swap
          swap (a, b)     = (b, a)
          mapFilter       = filter isJust . map (go (it+1))
          sortLen         = sortOn (negate . T.length . snd)
          filterSub       = filter ((`isSubstringOf` xs) . snd)

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
