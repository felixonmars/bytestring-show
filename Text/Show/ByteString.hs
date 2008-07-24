{-# LANGUAGE TypeSynonymInstances #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Text.Show.ByteString
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (type synonym instances)
--
-- Efficiently convert from values to lazy byte strings.

module Text.Show.ByteString ( -- * The Show class
                              Show (..)
                            , show
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
                              -- * Combining builders
                            , unlinesP
                            , unwordsP
                              -- * Printing values
                            , print
                            ) where

import Prelude hiding (Show(..), print, putStrLn)
import qualified Prelude

import Data.Binary.Put
import Data.ByteString.Lazy

import Data.Int
import Data.Word
import Data.Ratio
import Data.Complex

import Text.Show.ByteString.Util ( putAscii , putUTF8
                                 , putAsciiStr, putUTF8Str
                                 , unsafePutDigit
                                 )

import Text.Show.ByteString.Char
import Text.Show.ByteString.Int
import Text.Show.ByteString.Integer
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

-- | Encode a single value into a byte string
show :: Show a => a -> ByteString
show = runPut . showp

-- | Print a value to the standard output
print :: Show a => a -> IO ()
print = putStrLn . show

-- | Merge several string builders, separating them by newlines
unlinesP :: [Put] -> Put
unlinesP [    ] = return ()
unlinesP (p:ps) = p >> putAscii '\n' >> unlinesP ps

-- | Merge several string builders, separating them by spaces
unwordsP :: [Put] -> Put
unwordsP [    ] = return ()
unwordsP [p]    = p
unwordsP (p:ps) = p >> putAscii ' ' >> unwordsP ps

-- | Puts the digit corresponding to the Int passed in.
putDigit :: Int -> Put
putDigit i
  | i < 0     = error $ "putDigit: Negative integer: " ++ Prelude.show i
  | i > 9     = error $ "putDigit: Non-decimal digit: " ++ Prelude.show i
  | otherwise = unsafePutDigit i

-- This may be a bad idea, but I'm trying it out
instance Show Put where
  showp p = p

instance Show () where
  showp () = putAscii '(' >> putAscii ')'

instance Show Char where
  showp     = showpChar
  showpList = showpString

instance Show Bool where
  showp True  = putAsciiStr "True"
  showp False = putAsciiStr "False"

instance (Show a) => Show [a] where
  showp = showpList

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

instance Show Integer where
  showp = showpInteger

instance Show Float where
  showp = showpGFloat Nothing

instance Show Double where
  showp = showpGFloat Nothing

instance (Show a, Integral a) => Show (Ratio a) where
  showp q = wrap (numerator q) >>
            putAscii '%' >>
            showp (denominator q)
   where
   wrap n
     | n < 0     = putAscii '(' >> showp n >> putAscii ')'
     | otherwise = showp n

instance (Show a, RealFloat a) => Show (Complex a) where
  showp (a :+ b) = showp a >>
                   putAscii ' ' >> putAscii ':' >> putAscii '+' >> putAscii ' ' >>
                   showp b

instance Show a => Show (Maybe a) where
  showp Nothing  = putAsciiStr "Nothing"
  showp (Just a) = putAsciiStr "Just " >> showp a

instance (Show a, Show b) => Show (a,b) where
  showp (a,b) = putAscii '(' >> showp a >> putAscii ',' >> showp b >> putAscii ')'

instance (Show a, Show b, Show c) => Show (a,b,c) where
  showp (a,b,c) = putAscii '(' >> showp a >>
                  putAscii ',' >> showp b >>
                  putAscii ',' >> showp c >>
                  putAscii ')'

instance (Show a, Show b, Show c, Show d) => Show (a,b,c,d) where
  showp (a,b,c,d) =
    putAscii '(' >> showp a >>
    putAscii ',' >> showp b >>
    putAscii ',' >> showp c >>
    putAscii ',' >> showp d >>
    putAscii ')'

instance (Show a, Show b, Show c, Show d, Show e) => Show (a,b,c,d,e) where
  showp (a,b,c,d,e) =
    putAscii '(' >> showp a >>
    putAscii ',' >> showp b >>
    putAscii ',' >> showp c >>
    putAscii ',' >> showp d >>
    putAscii ',' >> showp e >>
    putAscii ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a,b,c,d,e,f) where
  showp (a,b,c,d,e,f) =
    putAscii '(' >> showp a >>
    putAscii ',' >> showp b >>
    putAscii ',' >> showp c >>
    putAscii ',' >> showp d >>
    putAscii ',' >> showp e >>
    putAscii ',' >> showp f >>
    putAscii ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (a,b,c,d,e,f,g) where
  showp (a,b,c,d,e,f,g) =
    putAscii '(' >> showp a >>
    putAscii ',' >> showp b >>
    putAscii ',' >> showp c >>
    putAscii ',' >> showp d >>
    putAscii ',' >> showp e >>
    putAscii ',' >> showp f >>
    putAscii ',' >> showp g >>
    putAscii ')'
