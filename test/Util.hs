{-# OPTIONS_GHC -Wno-orphans #-}

module Util
    ( module Prelude
    , module Control.Arrow
    , module Control.Monad
    , module Data.Function
    , module Data.Maybe
    , module Data.Ord
    , module Data.String
    , module Debug.Trace
    , module Test.Hspec
    , module Test.Hspec.QuickCheck
    , module Test.QuickCheck
    , (!?)
    , boundedAdd
    )
where

import Control.Arrow
import Control.Monad
import Data.Function
import Data.Maybe
import Data.Ord (clamp)
import Data.String
import Data.Text qualified as Strict
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Zipper (TextZipper)
import Data.Text.Lazy.Zipper qualified as TextZipper
import Data.Text.Rope (Rope)
import Data.Text.Rope qualified as Rope
import Data.Text.Rope.Zipper (RopeZipper)
import Data.Text.Rope.Zipper qualified as RopeZipper
import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude

instance Arbitrary Strict.Text where
    arbitrary = fromString <$> listOf (elements "abcdefg\n")

instance Arbitrary Lazy.Text where
    arbitrary = Lazy.fromStrict <$> arbitrary

instance Arbitrary Rope where
    arbitrary = Rope.fromText <$> arbitrary

instance Arbitrary TextZipper where
    arbitrary = TextZipper.fromParts <$> arbitrary <*> arbitrary

instance Arbitrary RopeZipper where
    arbitrary = RopeZipper.fromParts <$> arbitrary <*> arbitrary

infixr 9 !?

-- | Get element n of a list, or Nothing. Like `!!` but safe.
(!?) :: [a] -> Int -> Maybe a
(!?) xs i = listToMaybe $ drop i xs

boundedAdd :: Int -> Word -> Word
boundedAdd delta = fromIntegral . clamp @Int (0, maxBound) . (+ delta) . fromIntegral
