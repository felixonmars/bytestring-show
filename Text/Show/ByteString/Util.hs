{-# LANGUAGE MagicHash #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Text.Show.ByteString.Util
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (magic hash, GHC libraries)
--
-- Utility functions for producing readable byte strings.

module Text.Show.ByteString.Util ( putAscii
                                 , putUTF8
                                 , putAsciiStr
                                 , putUTF8Str
                                 , unsafePutDigit
                                 , unsafePutDigit#
                                 ) where

import GHC.Base
import GHC.Word

import Data.Binary
import Data.Bits

import Data.ByteString.Internal (c2w)

-- | Writes a single Char to a byte string, assuming it's ascii.
putAscii :: Char -> Put
putAscii = putWord8 . c2w

-- | Writes a single Char to a byte string, properly UTF-8 encoded
putUTF8 :: Char -> Put
putUTF8 c
  | oc <= 0x7f   = putWord8 (fromIntegral oc)
  | oc <= 0x7ff  = do putWord8 . fromIntegral $ 0xc0 + (oc `shiftR` 6)
                      putWord8 . fromIntegral $ 0x80 + oc .&. 0x3f
  | oc <= 0xffff = do putWord8 . fromIntegral $ 0xf0 + (oc `shiftR` 12)
                      putWord8 . fromIntegral $ 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                      putWord8 . fromIntegral $ 0x80 + oc .&. 0x3f
  | otherwise    = do putWord8 . fromIntegral $ 0xf0 + (oc `shiftR` 18)
                      putWord8 . fromIntegral $ 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                      putWord8 . fromIntegral $ 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                      putWord8 . fromIntegral $ 0x80 + oc .&. 0x3f
 where
 oc = ord c

-- | Writes a string of ascii characters to a byte string
putAsciiStr :: String -> Put
putAsciiStr = mapM_ putAscii

-- | Writes a string of unicode characters to a byte string,
-- properly UTF-8 encoded
putUTF8Str :: String -> Put
putUTF8Str = mapM_ putUTF8

-- | Puts the decimal digit corresponding to the given Int without
-- checking that it is in the interval [0,9]
unsafePutDigit :: Int -> Put
unsafePutDigit (I# i#) = unsafePutDigit# (int2Word# i#)

unsafePutDigit# :: Word# -> Put
unsafePutDigit# w# = putWord8 (W8# (w# `plusWord#` int2Word# 48#))
