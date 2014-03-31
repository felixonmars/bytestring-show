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

#if   __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ <  611 && INTEGER_GMP
import GHC.Integer.Internals
#elif __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 611 && INTEGER_GMP
import GHC.Integer.GMP.Internals
#elif __GLASGOW_HASKELL__ && INTEGER_SIMPLE
import GHC.Integer.Simple.Internals
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
#ifdef INTEGER_SIMPLE
#elif INTEGER_GMP
showpInteger (S# i#) = putI i#
#else
showpInteger (I# i#) = putI i#
#endif
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
#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
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
#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
  (# q, r #) ->
#else
  (q, r) ->
#endif
            q : r : splitb p ns

printh :: [Integer] -> Put
printh [    ] = error "printh: the impossible happened."
printh (n:ns) = case n `quotRemInteger` mx of
#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
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
#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
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

-- | Shows an Integral number using the base specified by the first
-- argument and the chracter representation specified by the second.
showpIntAtBase :: Integral a => a -> (Int -> Char) -> a -> Put
showpIntAtBase b f n | n < 0     = putAscii '-' >> showpIntAtBase b f (-n)
                     | n == 0    = putAscii (f 0)
                     | otherwise = let
  go k | k == 0    = return ()
       | otherwise = go d >> putAscii (f $ fromIntegral m)
   where
   (d, m) = k `divMod` b
  in go n
