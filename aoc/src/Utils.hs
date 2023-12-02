module Utils where

import Data.Attoparsec.ByteString.Char8 ( Parser, parseOnly )
import Data.ByteString.Char8 ( ByteString )

fromEither :: Either String a -> a
fromEither (Left s) = error s
fromEither (Right x) = x

runParser :: Parser a -> ByteString -> a
runParser p = fromEither . parseOnly p

applyFst :: ((a, b) -> c) -> (a, b) -> (c, b)
applyFst f (a, b) = (f (a, b), b)

applySnd :: ((a, b) -> c) -> (a, b) -> (a, c)
applySnd f (a, b) = (a, f (a, b))

both :: (a -> b) -> (a, a) -> (b, b) 
both f (a, b) = (f a, f b)
