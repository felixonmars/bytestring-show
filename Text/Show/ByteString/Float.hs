-- ---------------------------------------------------------------------------
-- |
-- Module      : Text.Show.ByteString.Float
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (magic hash)
--
-- Putting floating point values.
--
-- The code in this module is heavily based on GHC.Float

module Text.Show.ByteString.Float where

import GHC.Float

import Control.Monad

import Data.Binary

import Text.Show.ByteString.Util
import Text.Show.ByteString.Int

-- | Show a signed RealFloat value using decimal notation when the
-- absolute value lies between 0.1 and 9,999,999, and scientific
-- notation otherwise. The optional integer can be used to specify
-- precision.
showpGFloat :: RealFloat a => Maybe Int -> a -> Put
showpGFloat = putFormattedFloat FFGeneric

-- | Show a signed RealFloat value using decimal notation. The optional
-- integer can be used to specify precision.
showpFFloat :: RealFloat a => Maybe Int -> a -> Put
showpFFloat = putFormattedFloat FFFixed

-- | Show a signed RealFloat value using scientific (exponential) notation.
-- The optional integer can be used to specify precision.
showpEFloat :: RealFloat a => Maybe Int -> a -> Put
showpEFloat = putFormattedFloat FFExponent

putFormattedFloat :: RealFloat a => FFFormat -> Maybe Int -> a -> Put
putFormattedFloat fmt decs f
  | isNaN f                   = putAscii 'N' >> putAscii 'a' >> putAscii 'N'
  | isInfinite f              = putAsciiStr (if f < 0 then "-Infinity" else "Infinity")
  | f < 0 || isNegativeZero f = putAscii '-' >> go fmt (floatToDigits (toInteger base) (-f))
  | otherwise                 = go fmt (floatToDigits (toInteger base) f)
 where
 base = 10

 go FFGeneric p@(_,e)
   | e < 0 || e > 7 = go FFExponent p
   | otherwise      = go FFFixed    p
 go FFExponent (is, e) =
   case decs of
     Nothing -> case is of
       []     -> error "putFormattedFloat"
       [0]    -> putAsciiStr "0.0e0"
       [d]    -> unsafePutDigit d >> putAsciiStr ".0e" >> showpInt (e-1)
       (d:ds) -> unsafePutDigit d >> putAscii '.' >> mapM_ unsafePutDigit ds
                                  >> putAscii 'e' >> showpInt (e-1)
     Just dec ->
       let dec' = max dec 1 in
       case is of
         [0] -> putAscii '0' >> putAscii '.' >> replicateM_ dec' (putAscii '0')
                  >> putAscii 'e' >> putAscii '0'
         _   ->
           let (ei, is') = roundTo base (dec'+1) is
               (d:ds)    = if ei > 0 then init is' else is'
           in unsafePutDigit d >> putAscii '.' >> mapM_ unsafePutDigit ds
                >> putAscii 'e' >> showpInt (e - 1 + ei)
 go FFFixed (is, e) = case decs of
   Nothing
     | e <= 0    -> putAscii '0' >> putAscii '.' >> replicateM_ (-e) (putAscii '0')
                      >> mapM_ unsafePutDigit is
     | otherwise -> let g 0 rs     = putAscii '.' >> mk0 rs
                        g n []     = putAscii '0' >> g (n-1) []
                        g n (r:rs) = unsafePutDigit r >> g (n-1) rs
                    in g e is
   Just dec ->
     let dec' = max dec 0 in
     if e >= 0 then
       let (ei, is') = roundTo base (dec' + e) is
           (ls,rs)   = splitAt (e+ei) is'
       in mk0 ls >> when (not $ null rs) (putAscii '.' >> mapM_ unsafePutDigit rs)
     else
       let (ei, is') = roundTo base dec' (replicate (-e) 0 ++ is)
           d:ds      = if ei > 0 then is' else 0:is'
       in unsafePutDigit d >> when (not $ null ds) (putAscii '.' >> mapM_ unsafePutDigit ds)

 mk0 [] = putAscii '0'
 mk0 rs = mapM_ unsafePutDigit rs
