{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Text.Lazy.ZipperSpec where

import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Zipper
import Data.Text.Lazy.Zipper qualified as Zipper
import Util

data Move = Backward | Forward | Start | End | Rel !Int | Abs !Position
    deriving stock (Eq, Show)

instance Arbitrary Move where
    arbitrary =
        oneof
            [ pure Backward
            , pure Forward
            , pure Start
            , pure End
            , Rel <$> arbitrary
            , Abs <$> arbitrary
            ]

moveZipper :: Move -> TextZipper -> TextZipper
moveZipper Forward = moveForward
moveZipper Backward = moveBackward
moveZipper Start = moveStart
moveZipper End = moveEnd
moveZipper (Rel i) = moveCursor $ boundedAdd i
moveZipper (Abs i) = setCursor i

spec :: Spec
spec = parallel $ modifyMaxSuccess (* 100) do
    it "fromText is a right inverse of toText" $ property $ \t -> do
        let zipper = fromText t
        toText zipper `shouldBe` t
        Zipper.length zipper `shouldBe` Text.length t

    it "concatenates" $ property $ \t1 t2 -> do
        fromText t1 <> fromText t2 `shouldBe` fromParts t1 t2

    it "inserts" $ property $ \t1 t2 t3 ->
        insert t2 (fromParts t1 t3) `shouldBe` fromParts (t1 <> t2) t3

    it "deletes before" $ property $ \t1 (Text.singleton -> c) t2 ->
        deleteBefore (fromParts (t1 <> c) t2) `shouldBe` fromParts t1 t2

    it "deletes after" $ property $ \t1 (Text.singleton -> c) t2 ->
        deleteAfter (fromParts t1 (c <> t2)) `shouldBe` fromParts t1 t2

    it "moves" $ property $ \t1 t2 move -> do
        let zipper = fromParts t1 t2
        let zipperLength = fromIntegral . Zipper.length $ zipper
        zipper.cursor `shouldBe` fromIntegral (Text.length t1)
        zipper.cursor `shouldBe` fromIntegral (Text.length zipper.beforeCursor)
        let moveCursor' =
                clamp (0, zipperLength) . case move of
                    Backward -> boundedAdd (-1)
                    Forward -> boundedAdd 1
                    Start -> const minBound
                    End -> const maxBound
                    Rel i -> boundedAdd i
                    Abs i -> const i
        let zipper' = moveZipper move zipper
        zipper'.cursor `shouldBe` moveCursor' zipper.cursor
        zipper'.cursor `shouldBe` fromIntegral (Text.length zipper'.beforeCursor)
        toText zipper `shouldBe` toText zipper'
