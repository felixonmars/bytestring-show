
module Main where

import Test.QuickCheck

import Data.Complex
import Data.Int
import Data.Word
import Data.Ratio

import qualified Text.Show.ByteString as S

import qualified Data.ByteString.Lazy.Char8 as L

test_matchesShow :: (Show a, S.Show a, Arbitrary a) => a -> Bool
test_matchesShow a = show a == L.unpack (S.show a)

instance Arbitrary Int64 where
    arbitrary = fmap fromInteger arbitrary
    coarbitrary _ = id

instance Arbitrary Word64 where
    arbitrary = fmap (fromInteger . abs) arbitrary
    coarbitrary _ = id

test_all :: IO ()
test_all = do quickCheck (test_matchesShow :: Int -> Bool)
--              quickCheck (test_matchesShow :: Int8 -> Bool)
--              quickCheck (test_matchesShow :: Int16 -> Bool)
--              quickCheck (test_matchesShow :: Int32 -> Bool)
              quickCheck (test_matchesShow :: Int64 -> Bool)
--              quickCheck (test_matchesShow :: Word -> Bool)
--              quickCheck (test_matchesShow :: Word8 -> Bool)
--              quickCheck (test_matchesShow :: Word16 -> Bool)
--              quickCheck (test_matchesShow :: Word32 -> Bool)
              quickCheck (test_matchesShow :: Word64 -> Bool)
              quickCheck (test_matchesShow :: () -> Bool)
              quickCheck (test_matchesShow :: Integer -> Bool)
              quickCheck (test_matchesShow :: Float -> Bool)
              quickCheck (test_matchesShow :: Double -> Bool)
--              quickCheck (test_matchesShow :: Rational -> Bool)
              quickCheck (test_matchesShow :: (Int,Int,Int) -> Bool)
              quickCheck (test_matchesShow :: (Int,Int,Int,Int) -> Bool)
              quickCheck (test_matchesShow :: [Int] -> Bool)

main = test_all