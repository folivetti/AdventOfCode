{-# language OverloadedStrings #-}
module Y2015.Day23 (solution) where

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative ( (<|>) )
import Debug.Trace ( trace )

type Triple = (Int, Int, Int)

first, second, third :: (Int -> Int) -> Triple -> Triple
first f (a, b, c) = (f a, b, c)
second f (a, b, c) = (a, f b, c)
third f (a, b, c) = (a, b, f c)

getFst, getSnd :: Triple -> Int
getFst (a, _, _) = a
getSnd (_, b, _) = b

parseInstruction :: Parser (Triple -> Triple)
parseInstruction = hlf <|> tpl <|> inc <|> jmp <|> jie <|> jio 
  where
      hlf = string "hlf " >> parseReg >>= \f -> pure (f (`div` 2))
      tpl = string "tpl " >> parseReg >>= \f -> pure (f (*3))
      inc = string "inc " >> parseReg >>= \f -> pure (f (+1))
      jmp = string "jmp " >> signed decimal >>= \x -> pure (third (+x))
      jie = string "jie " >> getJmp even
      jio = string "jio " >> getJmp (==1)
      parseReg = (char 'a' >> pure first) <|> (char 'b' >> pure second)
      parseGet = (char 'a' >> pure getFst) <|> (char 'b' >> pure getSnd)

      getJmp g = do f <- parseGet
                    string ", "
                    x <- signed decimal
                    pure (\reg -> if g (f reg) then third (+x) reg else reg)


runParser :: Parser a -> B.ByteString -> a
runParser parser dat = case parse parser dat of
                  Done _ x -> x
                  Partial p -> case p "" of
                                 Done _ x -> x
                                 _ -> error "no parse"
                  _ -> error "no parse"
{-# INLINE runParser #-}

evalFuns :: [Triple -> Triple] -> Triple -> Triple
evalFuns funs = go
  where
    n = length funs
    go (a, b, ix)
      | ix >= n = (a, b, ix)
      | otherwise = let (a', b', ix') = (funs !! ix) (a, b, ix)
                     in if ix == ix' then go (a', b', ix+1)
                                     else go (a', b', ix')
solution :: IO ()
solution = do content <- B.lines <$> B.readFile "inputs/2015/input23.txt"
              let funs = map (runParser parseInstruction) content
              print $ evalFuns funs (0,0,0)
              print $ evalFuns funs (1,0,0)
