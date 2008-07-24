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

module Text.Show.ByteString.Int where

import GHC.Base
import GHC.Int
import GHC.Word

import Data.Binary

import Text.Show.ByteString.Util

putI :: Int# -> Put
putI i#
  | i# <# 0#  = let I# minInt# = minInt
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

showpWord :: Word -> Put
showpWord (W# w#) = putW w#

showpWord8 :: Word8 -> Put
showpWord8 (W8# w#) = putW w#

showpWord16 :: Word16 -> Put
showpWord16 (W16# w#) = putW w#

showpWord32 :: Word32 -> Put
showpWord32 (W32# w#) = putW w#