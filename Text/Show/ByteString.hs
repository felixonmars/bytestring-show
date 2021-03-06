{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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
                              -- * Putting integers
                            , showpIntAtBase
                              -- * Putting floats
                            , showpGFloat
                            , showpFFloat
                            , showpEFloat
                              -- * Combining builders
                            , unlinesP
                            , unwordsP
                            , showpParen
                              -- * Printing values
                            , print
                              -- * Put
                            , Put
                            , PutM(..)
                            , runPut
                            ) where

import Prelude hiding (Show(..), print, putStrLn)
import qualified Prelude

import Data.Binary.Put
import Data.ByteString.Lazy.Char8

import Data.Int
import Data.Word
import Data.Ratio
import Data.Complex

import Data.Array

import qualified Data.Map as M
import qualified Data.Set as S

import Text.Show.ByteString.Util ( putAscii , putUTF8
                                 , putAsciiStr, putUTF8Str
                                 , unsafePutDigit
                                 )

import Text.Show.ByteString.Char
import Text.Show.ByteString.Int
import Text.Show.ByteString.Integer
import Text.Show.ByteString.Float

-- | Conversion of values to readable byte strings.
-- Minimal complete definition: 'showp' or 'showpPrec'
class Show a where
  -- | Encodes a value to an efficient byte string builder.
  -- The precedence is used to determine where to put
  -- parentheses in a shown expression involving operators.
  --
  -- Values of type Put can be efficiently combined, so the
  -- showp functions are available for showing multiple values
  -- before producing an output byte string.
  showpPrec :: Int -> a -> Put

  -- | Encodes a value to an efficient byte string builder.
  -- Values of type Put can be efficiently combined, so this
  -- is available for building strings from multiple values.
  showp     :: a -> Put

  -- | Allows for specialized display of lists of values.
  -- This is used, for example, when showing arrays of Chars.
  showpList :: [a] -> Put

  showpPrec _ = showp

  showp = showpPrec 0

  showpList [] = putWord8 91 >> putWord8 93          -- "[]"
  showpList (x:xs) = putWord8 91 >> showp x >> go xs -- "[..
   where
   go (y:ys) = putWord8 44 >> showp y >> go ys       -- ..,..
   go [    ] = putWord8 93                           -- ..]"

-- | Encode a single value into a byte string
show :: Show a => a -> ByteString
show = runPut . showp

-- | A utility function for surrounding output by parentheses
-- conditionally.
showpParen :: Bool -> Put -> Put
showpParen b p | b         = putAscii '(' >> p >> putAscii ')'
               | otherwise = p

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

  showpPrec k i = showpParen (i < 0 && k > 0) $ showpInt i

instance Show Int8 where
  showp = showpInt8

  showpPrec k i = showpParen (i < 0 && k > 0) $ showpInt8 i

instance Show Int16 where
  showp = showpInt16

  showpPrec k i = showpParen (i < 0 && k > 0) $ showpInt16 i

instance Show Int32 where
  showp = showpInt32

  showpPrec k i = showpParen (i < 0 && k > 0) $ showpInt32 i

instance Show Int64 where
  showp = showpInt64

  showpPrec k i = showpParen (i < 0 && k > 0) $ showpInt64 i

instance Show Word where
  showp = showpWord

instance Show Word8 where
  showp = showpWord8

instance Show Word16 where
  showp = showpWord16

instance Show Word32 where
  showp = showpWord32

instance Show Word64 where
  showp = showpWord64

instance Show Integer where
  showp = showpInteger

  showpPrec k i = showpParen (i < 0 && k > 0) $ showpInteger i

instance Show Float where
  showp = showpGFloat Nothing

  showpPrec k f = showpParen (f < 0 && k > 0) $ showpGFloat Nothing f

instance Show Double where
  showp = showpGFloat Nothing

  showpPrec k f = showpParen (f < 0 && k > 0) $ showpGFloat Nothing f

instance (Show a, Integral a) => Show (Ratio a) where
  showpPrec k q = showpParen (k > 7) $ showpPrec 8 (numerator q) >>
                  putAscii '%' >> showp (denominator q)

instance (Show a, RealFloat a) => Show (Complex a) where
  showpPrec k (a :+ b) = showpParen (k > 6) $ showpPrec 7 a >>
                         putAscii ' ' >> putAscii ':' >> putAscii '+' >> putAscii ' ' >>
                         showpPrec 7 b

instance Show a => Show (Maybe a) where
  showpPrec _ Nothing  = putAsciiStr "Nothing"
  showpPrec k (Just a) = showpParen (k > 10) $ putAsciiStr "Just " >> showpPrec 11 a

instance (Show a, Show b) => Show (Either a b) where
  showpPrec k (Left a)  = showpParen (k > 10) $ putAsciiStr "Left " >> showpPrec 11 a
  showpPrec k (Right b) = showpParen (k > 10) $ putAsciiStr "Right " >> showpPrec 11 b

instance Show Ordering where
  showp LT = putAscii 'L' >> putAscii 'T'
  showp EQ = putAscii 'E' >> putAscii 'Q'
  showp GT = putAscii 'G' >> putAscii 'T'

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

instance (Show i, Show e, Ix i) => Show (Array i e) where
  showpPrec k a = showpParen (k > 10) $ putAsciiStr "array " >>
                  showp (bounds a) >> putAscii ' ' >> showp (assocs a)

instance (Show k, Show v) => Show (M.Map k v) where
  showpPrec k m = showpParen (k > 10) $ putAsciiStr "fromList " >>
                  showp (M.toList m)

instance (Show e) => Show (S.Set e) where
  showpPrec k s = showpParen (k > 10) $ putAsciiStr "fromList " >>
                  showp (S.toList s)

