-- |
-- Copyright: (C) 2015 Siddhanathan Shanmugam
-- License: GPL (see LICENSE)
-- Maintainer: siddhanathan@gmail.com
-- Portability: very
--
-- Concise Brainfuck Interpreter using ListZippers
-- This version uses the State Monad Transformer
--

module Main where

import Control.Applicative ((<$>))
import Control.Monad.State (StateT, get, put, evalStateT, unless, liftIO)
import Data.Char (ord, chr)
import Data.List.Zipper (Zipper, fromList, left, right, cursor, replace, beginp, endp)
import System.Environment (getArgs)

data Brainfuck = Brainfuck (Zipper Int) (Zipper Char) deriving Show
type BrainfuckM = StateT Brainfuck IO

bf :: BrainfuckM ()
bf = do
  Brainfuck zi zc <- get
  case (cursor zi, cursor zc) of
    (_, '>') -> put $ Brainfuck (right zi) (right zc)
    (_, '<') -> put $ Brainfuck (left zi)  (right zc)
    (_, '+') -> put $ Brainfuck (replace (cursor zi + 1) zi) (right zc)
    (_, '-') -> put $ Brainfuck (replace (cursor zi - 1) zi) (right zc)
    (_, '.') -> do { liftIO (putChar (chr $ cursor zi)); put $ Brainfuck zi (right zc) }
    (_, ',') -> liftIO getChar >>= \x -> put $ Brainfuck (replace (ord x) zi) (right zc)
    (n, '[') -> case n of 0 -> put $ Brainfuck zi (loopTraverse R (right zc))
                          _ -> put $ Brainfuck zi (right zc)
    (_, ']') -> put $ Brainfuck zi (loopTraverse L (left zc))
    _        -> put $ Brainfuck zi (right zc)
  Brainfuck _ zc2 <- get
  unless (endp zc2) bf

run :: String -> IO ()
run s = evalStateT bf (Brainfuck (fromList $ replicate 30000 0) (fromList s))

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

main :: IO ()
main = head <$> getArgs >>= readFile >>= run

