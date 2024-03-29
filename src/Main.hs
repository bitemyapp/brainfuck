-- |
-- Copyright: (C) 2015 Siddhanathan Shanmugam
-- License: GPL (see LICENSE)
-- Maintainer: siddhanathan@gmail.com
-- Portability: very
--
-- Concise Brainfuck Interpreter using ListZippers
--

module Main where

import Control.Applicative ((<$>))
import Data.Char (ord, chr)
import Data.List.Zipper (Zipper, fromList, left, right, cursor, replace, beginp, endp)
import System.Environment (getArgs)

data Direction = L | R deriving Eq

loopTraverse :: Direction -> Zipper Char -> Zipper Char
loopTraverse d z = l d z 0
  where l :: Direction -> Zipper Char -> Int -> Zipper Char
        l d z n = case (d, cursor z) of
          (L, ']') -> l d (left z) (n+1)
          (L, '[') -> if n == 0 then z else l d (left z) (n-1)
          (L,  _ ) -> l d (left z) n
          (R, '[') -> l d (right z) (n+1)
          (R, ']') -> if n == 0 then right z else l d (right z) (n-1)
          (R,  _ ) -> l d (right z) n


f :: Zipper Int -> Zipper Char -> IO ()
f zi zc
  | endp zc                            = return ()
  | cursor zc == '>'                   = f (right zi) (right zc)
  | cursor zc == '<'                   = f (left zi) (right zc)
  | cursor zc == '+'                   = f (replace (cursor zi + 1) zi) (right zc)
  | cursor zc == '-'                   = f (replace (cursor zi - 1) zi) (right zc)
  | cursor zc == '.'                   = putChar (chr $ cursor zi) >> f zi (right zc)
  | cursor zc == ','                   = getChar >>= \x -> f (replace (ord x) zi) (right zc)
  | cursor zc == '[' && cursor zi /= 0 = f zi (right zc)
  | cursor zc == '[' && cursor zi == 0 = f zi (loopTraverse R (right zc))
  | cursor zc == ']'                   = f zi (loopTraverse L (left zc))
  | otherwise                          = f zi (right zc)

main :: IO ()
main = head <$> getArgs >>= readFile >>= \x -> f (fromList $ replicate 30000 0) (fromList x)

