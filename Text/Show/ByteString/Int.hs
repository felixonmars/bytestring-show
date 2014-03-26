{-# OPTIONS_GHC -funbox-strict-fields -cpp #-}
{-# LANGUAGE MagicHash #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Text.Show.ByteString.Int
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (magic hash)
--
-- Putting integers and words.
--
-- The code in this module is based on the printing in the GHC modules.

#include "MachDeps.h"

module Text.Show.ByteString.Int where

import GHC.Base
import GHC.Int
import GHC.Word

import Data.Binary

import Text.Show.ByteString.Util

putI :: Int# -> Put
putI i#
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 611
  | i# <# 0#  = let !(I# minInt#) = minInt
#else
  | i# <# 0#  = let I# minInt# = minInt
#endif
                in if i# ==# minInt#
                   then putWord8 45 >> putW (int2Word# (negateInt# (i# `quotInt#` 10#)))
                                    >> putW (int2Word# (negateInt# (i# `remInt#` 10#)))
                   else putWord8 45 >> putW (int2Word# (negateInt# i#))
  | otherwise = putW (int2Word# i#)

putW :: Word# -> Put
putW w#
  | w# `ltWord#` int2Word# 10# = unsafePutDigit# w#
  | otherwise                  = putW (w# `quotWord#` int2Word# 10#)
                              >> unsafePutDigit# (w# `remWord#` int2Word# 10#)

showpInt :: Int -> Put
showpInt (I# i#) = putI i#

showpInt8 :: Int8 -> Put
showpInt8 (I8# i#) = putI i#

showpInt16 :: Int16 -> Put
showpInt16 (I16# i#) = putI i#

showpInt32 :: Int32 -> Put
showpInt32 (I32# i#) = putI i#

showpInt64 :: Int64 -> Put
#if WORD_SIZE_IN_BITS >= 64
showpInt64 (I64# i#) = putI i#
#else /* WORD_SIZE_IN_BITS < 64 */
showpInt64 = putI64

-- Unboxed 64-bit-specific operations aren't exported

putI64 :: Int64 -> Put
putI64 i | i == minBound = putWord8 45
                           >> putW64 (fromIntegral $ negate (i `quot` 10))
                           >> putW64 (fromIntegral $ negate (i `rem` 10))
         | i < 0         = putWord8 45 >> putW64 (fromIntegral $ negate i)
         | otherwise     = putW64 (fromIntegral i)
#endif

showpWord :: Word -> Put
showpWord (W# w#) = putW w#

showpWord8 :: Word8 -> Put
showpWord8 (W8# w#) = putW w#

showpWord16 :: Word16 -> Put
showpWord16 (W16# w#) = putW w#

showpWord32 :: Word32 -> Put
showpWord32 (W32# w#) = putW w#

showpWord64 :: Word64 -> Put
#if WORD_SIZE_IN_BITS >= 64
showpWord64 (W64# w#) = putW w#
#else /* WORD_SIZE_IN_BITS < 64 */
showpWord64 = putW64

putW64 :: Word64 -> Put
putW64 w | w < 10    = unsafePutDigit64 w
         | otherwise = putW64 (w `quot` 10)
                       >> unsafePutDigit64 (w `rem` 10)
    where unsafePutDigit64 w = unsafePutDigit# (case fromIntegral w of (W# w#) -> w#)

#endif
