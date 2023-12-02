{-# language OverloadedStrings #-}
module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Utils ( runParser )
import Control.Applicative ( (<|>) )
import qualified Data.Map as M

type Value = Either Double (String, String, Char)

parser :: Parser (M.Map String Value)
parser = do key <- manyTill anyChar (string ": ")
            val <- eitherP (fromInteger <$> decimal) ops
            pure $ M.singleton key val
  where
    ops = operator '+' <|> operator '-' <|> operator '*' <|> operator '/'
    operator c = do l <- manyTill anyChar (char ' ')
                    o <- anyChar
                    char ' '
                    r <- many' anyChar
                    pure (l, r, o)

evalOp :: Char -> Double -> Double -> Double
evalOp '+' = (+)
evalOp '-' = (-)
evalOp '*' = (*)
evalOp '/' = (/)
evalOp  _  = undefined

evalTree :: M.Map String Value -> Double
evalTree tree = go (tree M.! "root")
  where
    go (Left  n)          = n
    go (Right (l, r, op)) = evalOp op (go (tree M.! l)) (go (tree M.! r))

solve :: M.Map String Value -> Double
solve tree = fromLeft $ go "root"
  where
    fromLeft (Left x) = x
    invOpR '+' x = subtract x
    invOpR '*' x = (/ x)
    invOpR '-' x = (+x)
    invOpR '/' x = (*x)
    
    invOpL '+' x = subtract x
    invOpL '*' x = (/ x)
    invOpL '-' x = negate . subtract x
    invOpL '/' x = recip . (/ x)
    
    go "root" =
        let Right (l, r, op) = tree M.! "root"
        in case (go l, go r) of
             (Left x, Right f) -> Left $ f x
             (Right f, Left x) -> Left $ f x
             _                 -> undefined
    go "humn" = Right id
    go node =
        case tree M.! node of
          Left n -> Left n
          Right (l, r, op) -> case (go l, go r) of
                                (Left x, Left y) -> Left (evalOp op x y)
                                (Right f, Left y) -> Right (f . invOpR op y)
                                (Left x, Right f) -> Right (f . invOpL op x)
                                _                 -> undefined

main :: IO ()
main = do content <- M.unions . map (runParser parser) . B.lines <$> B.readFile "inputs/2022/input21.txt"
          let Right (l, r, op) = content M.! "root"
              content' = M.insert "root" (Right (l, r, '-')) content
          print $ round $ evalTree content
          print $ round $ solve content'
