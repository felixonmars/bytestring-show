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
                            ) where

import Prelude hiding (Show(..))
import qualified Prelude

import Data.Binary

import Text.Show.ByteString.Util ( putAscii , putUTF8
                                 , putAsciiStr, putUTF8Str
                                 , unsafePutDigit
                                 )

import Text.Show.ByteString.Char

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