module Util where

import Data.Ord (clamp)
import Prelude

boundedSucc :: (Eq a, Bounded a, Enum a) => a -> a
boundedSucc e
    | e == maxBound = e
    | otherwise = succ e

boundedPred :: (Eq a, Bounded a, Enum a) => a -> a
boundedPred e
    | e == minBound = e
    | otherwise = pred e

boundedAdd :: Int -> Word -> Word
boundedAdd delta = fromIntegral . clamp @Int (0, maxBound) . (+ delta) . fromIntegral

absDelta :: (Num a, Ord a) => a -> a -> a
absDelta a b
    | a > b = a - b
    | otherwise = b - a
