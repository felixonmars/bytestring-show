-- ---------------------------------------------------------------------------
-- |
-- Module      : Text.Show.ByteString
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Portable
--
-- Efficiently convert from values to lazy byte strings.

module Text.Show.ByteString ( -- * The Show class
                              Show (..)
                              -- * Putting Chars
                            , putAscii
                            , putUTF8
                              -- * Putting Strings
                            , putAsciiStr
                            , putUTF8Str
                              -- * Putting digits
                            , unsafePutDigit
                            , putDigit
                              -- * Putting floats
                            , showpGFloat
                            , showpFFloat
                            , showpEFloat
                            ) where

import Prelude hiding (Show(..))
import qualified Prelude

import Data.Binary

import Data.Int
import Data.Word

import Text.Show.ByteString.Util ( putAscii , putUTF8
                                 , putAsciiStr, putUTF8Str
                                 , unsafePutDigit
                                 )

import Text.Show.ByteString.Char
import Text.Show.ByteString.Int
import Text.Show.ByteString.Float

-- | Conversion of values to readable byte strings.
-- Minimal complete definition: 'showp'
class Show a where
  -- | Encodes a value to an efficient byte string builder.
  -- Values of type Put can be efficiently combined, so this
  -- is available for building strings from multiple values.
  showp     :: a -> Put

  -- | Allows for specialized display of lists of values.
  -- This is used, for example, when showing arrays of Chars.
  showpList :: [a] -> Put

  showpList [] = putWord8 91 >> putWord8 93          -- "[]"
  showpList (x:xs) = putWord8 91 >> showp x >> go xs -- "[..
   where
   go (y:ys) = putWord8 44 >> showp y >> go ys       -- ..,..
   go [    ] = putWord8 93                           -- ..]"

-- | Puts the digit corresponding to the Int passed in.
putDigit :: Int -> Put
putDigit i
  | i < 0     = error $ "putDigit: Negative integer: " ++ Prelude.show i
  | i > 9     = error $ "putDigit: Non-decimal digit: " ++ Prelude.show i
  | otherwise = unsafePutDigit i

instance Show Char where
  showp     = showpChar
  showpList = showpString

instance (Show a) => Show [a] where
  showp     = showpList

instance Show Int where
  showp = showpInt

instance Show Int8 where
  showp = showpInt8

instance Show Int16 where
  showp = showpInt16

instance Show Int32 where
  showp = showpInt32

instance Show Word where
  showp = showpWord

instance Show Word8 where
  showp = showpWord8

instance Show Word16 where
  showp = showpWord16

instance Show Word32 where
  showp = showpWord32

instance Show Float where
  showp = showpGFloat Nothing

instance Show Double where
  showp = showpGFloat Nothing