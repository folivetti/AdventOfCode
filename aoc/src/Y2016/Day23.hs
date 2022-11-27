{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Y2016.Day23 ( solution ) where

import Utils ( runParser )
import Data.Attoparsec.ByteString.Char8 ( anyChar, space, decimal, signed, string, Parser )
import qualified Data.ByteString.Char8 as B
import Control.Applicative ( (<|>) )
import Control.Lens ( (+~), (-~), makeLenses, (^.), (.~), (&), element, over )
import Control.Monad.State 
import Debug.Trace ( trace )

data Reg = Reg { _a :: Int
               , _b :: Int
               , _c :: Int
               , _d :: Int
               , _ix :: Int
               } deriving (Show, Eq)

makeLenses ''Reg

toAccess :: Functor f => Char -> (Int -> f Int) -> Reg -> f Reg
toAccess 'a' = a
toAccess 'b' = b
toAccess 'c' = c
toAccess 'd' = d
toAccess  x  = error $ "unknown reg " <> [x]

parseReg :: Functor f => Parser ((Int -> f Int) -> Reg -> f Reg)
parseReg = toAccess <$> anyChar

parseCpy :: Parser (Reg -> State [B.ByteString] Reg)
parseCpy = parseCpyVal <|> parseCpyReg
  where
    parseCpyVal = do string "cpy "
                     x <- signed decimal
                     space
                     r <- parseReg
                     pure $ pure . (ix +~ 1) . (r .~ x)
                                     
    parseCpyReg = do string "cpy "
                     r1 <- parseReg
                     space
                     r2 <- parseReg
                     pure $ pure . (ix +~ 1) . (\reg -> (r2 .~) (reg ^. r1) reg)

parseInc :: Parser (Reg -> State [B.ByteString] Reg)
parseInc = do string "inc "
              r <- parseReg
              pure $ pure . (ix +~ 1) . (r +~ 1)
parseDec :: Parser (Reg -> State [B.ByteString] Reg)
parseDec = do string "dec "
              r <- parseReg
              pure $ pure . (ix +~ 1) . (r -~ 1)

parseJnz :: Parser (Reg -> State [B.ByteString] Reg)
parseJnz = parseJnzVal <|> parseJnzReg <|> parseJnzReg2

parseJnzVal :: Parser (Reg -> State [B.ByteString] Reg)
parseJnzVal = do string "jnz "
                 x <- signed decimal
                 space
                 y <- signed decimal
                 pure $ pure . if x==0 then (ix +~ 1) else (ix +~ y)

parseJnzReg :: Parser (Reg -> State [B.ByteString] Reg)
parseJnzReg = do string "jnz "
                 r <- parseReg
                 space
                 y <- signed decimal
                 pure $ \reg -> if (reg ^. r) == 0
                                  then pure $ (ix +~ 1) reg
                                  else pure $ (ix +~ y) reg
parseJnzReg2 :: Parser (Reg -> State [B.ByteString] Reg)
parseJnzReg2 = do string "jnz "
                  y <- signed decimal
                  space
                  r <- parseReg
                  pure $ \reg -> if y == 0 
                                   then pure $ (ix +~ 1) reg
                                   else pure $ (ix +~ (reg ^. r)) reg

parseTgl :: Parser (Reg -> State [B.ByteString] Reg)
parseTgl = do string "tgl "
              x <- parseReg
              pure $ \reg -> do let ix' = (reg ^. x) + (reg ^. ix)
                                instrs <- gets (map B.unpack)
                                if ix' >= length instrs
                                  then pure $ (ix +~ 1) reg 
                                  else do let new_instr = replaceInstr $ instrs !! ix'
                                          put $ map B.pack (instrs & element ix' .~ new_instr)
                                          pure $ (ix +~ 1) reg

parseAdd :: Parser (Reg -> State [B.ByteString] Reg)
parseAdd = do string "add "
              x <- parseReg
              space
              y <- parseReg
              space
              z <- parseReg
              pure $ \reg -> do let val = reg ^. y
                                pure $ (ix +~ 1) . (z .~ 0) . (x +~ val) $ reg
parseMul :: Parser (Reg -> State [B.ByteString] Reg)
parseMul = do string "mul "
              x <- parseReg
              space
              y <- parseReg
              space
              z <- parseReg
              pure $ \reg -> do let val = (reg ^. y) * (reg ^. z)
                                pure $ (ix +~ 1) . (x .~ val) $ reg

replaceInstr inst 
  | all (uncurry (==)) $ zip inst "inc" = "dec" <> drop 3 inst
  | all (uncurry (==)) $ zip inst "dec" = "inc" <> drop 3 inst
  | all (uncurry (==)) $ zip inst "tgl" = "inc" <> drop 3 inst
  | all (uncurry (==)) $ zip inst "jnz" = "cpy" <> drop 3 inst
  | all (uncurry (==)) $ zip inst "cpy" = "jnz" <> drop 3 inst
  | otherwise = error inst


parser :: Parser (Reg -> State [B.ByteString] Reg)
parser = parseCpy <|> parseInc <|> parseDec <|> parseJnz <|> parseTgl <|> parseAdd <|> parseMul

compute :: Int -> Reg -> [B.ByteString] -> Reg
compute n = go
  where
    go :: Reg -> [B.ByteString] -> Reg
    go reg instrs
      | reg ^. ix >= n = reg
      | otherwise      = let st = runParser parser (instrs !! _ix reg) reg 
                          in uncurry go $ runState st instrs

solution :: IO ()
solution = do content  <- B.lines <$> B.readFile "inputs/2016/input23.txt"
              content2 <- B.lines <$> B.readFile "inputs/2016/input23a.txt"
              print $ compute (length content) (Reg 7 0 0 0 0) content
              print $ product [12, 11 .. 2] + 93*80
