{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Text.Rope.ZipperSpec where

import Data.Text qualified as Text
import Data.Text.Lazy.Zipper qualified as TextZipper
import Data.Text.Rope qualified as Rope
import Data.Text.Rope.Zipper
import Data.Text.Rope.Zipper qualified as RopeZipper
import Util

rawOutput :: (HasCallStack) => Property -> IO ()
rawOutput =
    quickCheckWithResult stdArgs >=> \case
        Success{} -> pure ()
        Failure{output} -> expectationFailure output
        GaveUp{output} -> expectationFailure output
        NoExpectedFailure{output} -> expectationFailure output

data Move = Backward | Forward | Up | Down | Start | End | Rel !Cursor | Abs !Cursor
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

moveZipper :: Move -> RopeZipper -> RopeZipper
moveZipper Forward = moveForward
moveZipper Backward = moveBackward
moveZipper Up = moveUp
moveZipper Down = moveDown
moveZipper Start = moveToLineStart
moveZipper End = moveToLineEnd
moveZipper (Rel (dy, dx)) = moveCursor $ \(y, x) -> (y + dy, x + dx)
moveZipper (Abs c) = setCursor c

spec :: Spec
spec = parallel $ modifyMaxSuccess (* 100) do
    it "fromText is a right inverse of toText" $ property $ \t -> do
        toText (fromText t) `shouldBe` t

    it "fromRope is a right inverse of toRope" $ property $ \r -> do
        toRope (fromRope r) `shouldBe` r

    it "concatenates" $ property $ \r1 r2 -> do
        fromRope r1 <> fromRope r2 `shouldBe` fromParts r1 r2
        toRope (fromParts r1 r2) `shouldBe` r1 <> r2

    it "concatenates newline" $ property $ \zipper -> do
        splitFirstLine "\n" `shouldBe` ("\n", "")
        toRope (zipper <> "\n") `shouldBe` toRope zipper <> "\n"

    it "inserts" $ property $ \r1 r2 r3 -> do
        insertRope r2 (fromParts r1 r3) `shouldBe` fromParts (r1 <> r2) r3

    it "deletes before" $ property $ \r1 (Text.singleton -> Rope.fromText -> c) r2 ->
        deleteBefore (fromParts (r1 <> c) r2) `shouldBe` fromParts r1 r2

    it "deletes after" $ property $ \r1 (Text.singleton -> Rope.fromText -> c) r2 ->
        deleteAfter (fromParts r1 (c <> r2)) `shouldBe` fromParts r1 r2

    it "moves" $ property $ \t1 t2 move -> do
        let zipper = fromParts (Rope.fromText t1) (Rope.fromText t2)
            row = Text.count "\n" t1
            col = Text.length . snd $ Text.breakOnEnd "\n" t1
        zipper.cursor `shouldBe` (row, col)
        let t = t1 <> t2
            numLines = Text.count "\n" t + 1
            clampRow = clamp (0, numLines - 1)
            lineLength y = maybe 0 Text.length $ Text.lines t !? y
            clampCol (clampRow -> y, x) = (y, clamp (0, lineLength y) x)
            moveCursor' (y, x) = clampCol $ case move of
                Backward -> (y, x - 1)
                Forward -> (y, x + 1)
                Up -> (y - 1, x)
                Down -> (y + 1, x)
                Start -> (y, minBound)
                End -> (y, maxBound)
                Rel (dy, dx) -> (y + dy, x + dx)
                Abs (y, x) -> (y, x)
        let zipper' = moveZipper move zipper
        zipper'.cursor `shouldBe` moveCursor' zipper.cursor
        toRope zipper `shouldBe` toRope zipper'

    it "counts lines correctly" $ property $ \zipper -> do
        lengthInLines zipper `shouldBe` (fromIntegral . length . RopeZipper.lines) zipper

    it "sticks the last requested column" $ property $ \zipper -> do
        let test move = do
                let (row, col) = zipper.cursor
                let zipper' = moveZipper move zipper
                let (_, col') = zipper'.cursor
                col' `shouldBe` min col (fromIntegral $ TextZipper.length zipper'.currentLine)
                moveCursor (first $ const row) zipper' `shouldBe` zipper
        test Up
        test Down
