{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Bits
import qualified Data.Map as M 
import Utils
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Control.Monad.State.Strict
import Control.Monad
import Data.List ( sort, nub, intercalate )

data Op = AND | OR | XOR deriving (Show, Eq) 

type Value = Either Int (String, String, Op)
type Circuit = M.Map String Value 

parser :: Parser Circuit    
parser = do inputs <- many' parseInput
            endOfLine
            gates <- many' parseGate
            return (M.fromList (inputs <> gates))

parseInput :: Parser (String, Value)
parseInput = do name <- parseName
                string ": "
                value <- decimal
                endOfLine
                return (name, Left value)

parseName :: Parser String
parseName = many1 (satisfy isValidChar)

isValidChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

parseGate :: Parser (String, Value)
parseGate = do input1 <- parseName
               op <- parseOp
               input2 <- parseName
               string " -> "
               output <- parseName
               endOfLine
               return (output, Right (input1, input2, op))

parseOp :: Parser Op
parseOp = do string " AND " *> return AND -- (.&.)
         <|> string " OR " *> return OR -- (.|.)
         <|> string " XOR " *> return XOR -- xor

isOutput :: String -> Bool
isOutput ('z':_) = True
isOutput _ = False

evalOp :: Op -> Int -> Int -> Int
evalOp AND = (.&.)
evalOp OR = (.|.)
evalOp XOR = xor 

getOutputs :: [String] -> State Circuit Int
getOutputs outputs = foldM (\acc output -> do
                      value <- getValue output
                      return (shiftL acc 1 .|. value)) 0 $ reverse outputs

getValue :: String -> State Circuit Int
getValue name = do val <- gets (M.lookup name)
                   case val of
                     Just (Left value) -> return value
                     Just (Right (input1, input2, op)) -> do
                       value1 <- getValue input1
                       value2 <- getValue input2
                       let value = evalOp op value1 value2
                       modify (M.insert name (Left value))
                       return value
                     Nothing -> error "Unknown gate"

solve1 circuit = evalState (getOutputs outputs) circuit
  where
    outputs = sort $ M.keys $ M.filterWithKey (\k _ -> isOutput k) circuit

solve2 circuit = intercalate "," $ sort $ nub $ concatMap getWrongs $ M.toList circuit
  where 
      getOR                    = M.toList . M.filter (\v -> isRight v && getOp v == OR)
      getNotOR                 = M.toList . M.filter (\v -> isRight v && getOp v /= OR)
      isRight (Right _)        = True
      isRight _                = False
      getOp (Right (_, _, op)) = op
      outputs                  = sort $ M.keys $ M.filterWithKey (\k _ -> isOutput k) circuit

      getWrongs (k, Right (i1, i2, op)) 
        | head k == 'z' && op /= XOR && k /= last outputs = [k]
        | op == XOR && all (\i -> head i `notElem` ("xyz" :: String)) [k, i1, i2] = [k]
        | op == XOR = [k | (_, Right (i1', i2', _)) <- getOR circuit, k == i1' || k == i2']
        | op == AND && "x00" `notElem` [i1, i2] = [k | (_, Right (i1', i2', _)) <- getNotOR circuit, k == i1' || k == i2']
        | otherwise = []
      getWrongs _ = []



solve = solve1 &&& solve2 

f &&& g = \x -> (f x, g x) 

main :: IO ()
main = solve . runParser parser <$> B.readFile "inputs/2024/input24.txt"
         >>= print
         
