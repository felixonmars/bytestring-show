{-# LANGUAGE UnboxedTuples, MagicHash, BangPatterns, CPP #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Text.Show.ByteString.Integer
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (magic hash, bang patterns)
--
-- Putting unbounded integers.
--
-- This code is based off the integer showing code in GHC.

module Text.Show.ByteString.Integer where

import GHC.Base

#ifdef INTEGER_GMP
import GHC.Integer.Internals
#endif

import GHC.Num


import Data.Binary.Put

import Text.Show.ByteString.Util
import Text.Show.ByteString.Int

mx :: Integer
ds :: Int
(mx, ds) = until ((>mi) . (*10) . fst) (\(n,d) -> (n*10,d+1)) (10,1)
 where mi = fromIntegral (maxBound :: Int)

showpInteger :: Integer -> Put
showpInteger (S# i#) = putI i#
showpInteger n
  | n < 0     = putAscii '-' >> posIntegerPut (-n)
  | otherwise = posIntegerPut n

posIntegerPut :: Integer -> Put
posIntegerPut n
  | n < mx    = case fromInteger n of
                  I# i# -> putI i#
  | otherwise = printh (splitf (mx*mx) n)

splitf :: Integer -> Integer -> [Integer]
splitf p n
  | p > n     = [n]
  | otherwise = splith p (splitf (p*p) n)

splith :: Integer -> [Integer] -> [Integer]
splith _ [    ] = error "splith: the impossible happened."
splith p (n:ns) = case n `quotRemInteger` p of
#ifdef INTEGER_GMP
  (# q, r #) ->
#else
  (q, r) -> 
#endif
          if q > 0
            then q : r : splitb p ns
            else r : splitb p ns

splitb :: Integer -> [Integer] -> [Integer]
splitb _ [    ] = []
splitb p (n:ns) = case n `quotRemInteger` p of
#ifdef INTEGER_GMP
  (# q, r #) ->
#else
  (q, r) ->
#endif
            q : r : splitb p ns

printh :: [Integer] -> Put
printh [    ] = error "printh: the impossible happened."
printh (n:ns) = case n `quotRemInteger` mx of
#ifdef INTEGER_GMP
  (# q', r' #) ->
#else
  (q', r') ->
#endif
              let q = fromInteger q'
                  r = fromInteger r'
              in if q > 0 then phead q >> pblock r >> printb ns
                          else phead r >> printb ns

printb :: [Integer] -> Put
printb [    ] = return ()
printb (n:ns) = case n `quotRemInteger` mx of
#ifdef INTEGER_GMP
  (# q', r' #) ->
#else
  (q', r') ->
#endif
              let q = fromInteger q'
                  r = fromInteger r'
              in pblock q >> pblock r >> printb ns

phead :: Int -> Put
phead (I# i#) = putI i#

pblock :: Int -> Put
pblock = pblock' ds

pblock' :: Int -> Int -> Put
pblock' d !n
  | d == 1    = unsafePutDigit n
  | otherwise = pblock' (d-1) q >> unsafePutDigit r
 where (q, r) = n `quotRemInt` 10
