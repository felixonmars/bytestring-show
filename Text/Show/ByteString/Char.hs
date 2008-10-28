-- ---------------------------------------------------------------------------
-- |
-- Module      : Text.Show.ByteString.Char
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Portable
--
-- Putting characters.
--
-- Functions based on GHC.Show in base

module Text.Show.ByteString.Char where

import Data.Binary
import Data.Char

import Text.Show.ByteString.Util

asciiTab :: [String]
asciiTab = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US", "SP"]

putLitChar :: Char -> Put
putLitChar '\DEL' = putAsciiStr "\\DEL"
putLitChar '\\'   = putAscii '\\' >> putAscii '\\'
putLitChar c | c >= ' ' = putUTF8 c
putLitChar '\a' = putAscii '\\' >> putAscii 'a'
putLitChar '\b' = putAscii '\\' >> putAscii 'b'
putLitChar '\f' = putAscii '\\' >> putAscii 'f'
putLitChar '\n' = putAscii '\\' >> putAscii 'n'
putLitChar '\r' = putAscii '\\' >> putAscii 'r'
putLitChar '\t' = putAscii '\\' >> putAscii 't'
putLitChar '\v' = putAscii '\\' >> putAscii 'v'
putLitChar '\SO' = putAscii '\\' >> putAscii 'S' >> putAscii 'O'
putLitChar c     = putAscii '\\' >> putAsciiStr (asciiTab !! ord c)

showpChar :: Char -> Put
showpChar c = putAscii '\'' >> putEscaped c >> putAscii '\''
 where
 putEscaped '\'' = putAscii '\\' >> putAscii '\''
 putEscaped c' = putLitChar c'

showpString :: String -> Put
showpString xs = putAscii '"' >> mapM_ putEscaped xs >> putAscii '"'
 where
 putEscaped '"'   = putAscii '\\' >> putAscii '"'
 putEscaped '\SO' = putAsciiStr "\\SO\\&"
 putEscaped c     = putLitChar c
