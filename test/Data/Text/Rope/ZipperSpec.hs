{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Text.Rope.ZipperSpec where

import Data.Text qualified as Text
import Data.Text.Lazy.Zipper qualified as TextZipper
import Data.Text.Rope qualified as Rope
import Data.Text.Rope.Zipper
import Data.Text.Rope.Zipper qualified as RopeZipper
import Util
import Data.Foldable (foldl')

rawOutput :: (HasCallStack) => Property -> IO ()
rawOutput =
    quickCheckWithResult stdArgs >=> \case
        Success{} -> pure ()
        Failure{output} -> expectationFailure output
        GaveUp{output} -> expectationFailure output
        NoExpectedFailure{output} -> expectationFailure output

data Move
    = Backward
    | Forward
    | Up
    | Down
    | LineStart
    | LineEnd
    | FirstLine
    | LastLine
    | Rel !(Int, Int)
    | Abs !Position
    deriving stock (Eq, Show)

instance Arbitrary Position where
    arbitrary = do
        posLine <- arbitrary
        posColumn <- arbitrary
        pure Position{..}

instance Arbitrary Move where
    arbitrary =
        oneof
            [ pure Backward
            , pure Forward
            , pure LineStart
            , pure LineEnd
            , Rel <$> arbitrary
            , Abs <$> arbitrary
            ]

newtype VerticalMove = VerticalMove Move
    deriving newtype (Eq, Show)

instance Arbitrary VerticalMove where
    arbitrary = VerticalMove <$> elements [Up, Down, FirstLine, LastLine]

moveZipper :: [Move] -> RopeZipper -> RopeZipper
moveZipper = flip . foldl' $ flip \case
    Forward -> moveForward
    Backward -> moveBackward
    Up -> moveUp
    Down -> moveDown
    LineStart -> moveToLineStart
    LineEnd -> moveToLineEnd
    FirstLine -> moveToFirstLine
    LastLine -> moveToLastLine
    (Rel (dy, dx)) -> moveCursor \Position{..} ->
        Position
            { posLine = boundedAdd dy posLine
            , posColumn = boundedAdd dx posColumn
            }
    (Abs c) -> setCursor c

positionToPair :: Position -> (Int, Int)
positionToPair Position{..} = (fromIntegral posLine, fromIntegral posColumn)

pairToPosition :: (Int, Int) -> Position
pairToPosition (y, x) =
    Position
        { posLine = fromIntegral $ max 0 y
        , posColumn = fromIntegral $ max 0 x
        }

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
            cursor =
                Position
                    { posLine = fromIntegral $ Text.count "\n" t1
                    , posColumn = fromIntegral . Text.length . snd $ Text.breakOnEnd "\n" t1
                    }
        zipper.cursor `shouldBe` cursor
        let t = t1 <> t2
            numLines = Text.count "\n" t + 1
            clampRow = clamp (0, numLines - 1)
            lineLength y = maybe 0 Text.length $ Text.lines t !? y
            clampCol (clampRow -> y, x) = (y, clamp (0, lineLength y) x)
            moveCursor' (positionToPair -> (y, x)) = pairToPosition . clampCol $ case move of
                Backward -> (y, x - 1)
                Forward -> (y, x + 1)
                Up -> (y - 1, x)
                Down -> (y + 1, x)
                LineStart -> (y, minBound)
                LineEnd -> (y, maxBound)
                FirstLine -> (minBound, x)
                LastLine -> (maxBound, x)
                Rel (dy, dx) -> (y + dy, x + dx)
                Abs pos -> positionToPair pos
        let zipper' = moveZipper [move] zipper
        zipper'.cursor `shouldBe` moveCursor' zipper.cursor
        toRope zipper `shouldBe` toRope zipper'

    it "counts lines correctly" $ property $ \zipper -> do
        lengthInLines zipper
            `shouldBe` (fromIntegral . length . RopeZipper.lines) zipper

    it "sticks the last requested column" $ property $ \zipper (VerticalMove move) -> do
        let zipper' = moveZipper [move] zipper
        let lineLen' =
                fromIntegral $
                    TextZipper.length zipper'.currentLine
                        - if TextZipper.hasTrailingNewline zipper'.currentLine then 1 else 0
        let expectedColumn = min zipper.cursor.posColumn lineLen'
        zipper'.cursor.posColumn `shouldBe` expectedColumn
        moveCursor (\p -> p{posLine = zipper.cursor.posLine}) zipper' `shouldBe` zipper

    it "unsticks the last requested column" do
        let zipper = fromText "main :: IO ()\nmain = pure ()"
            lefts = 4 :: Int
            ups = 1 :: Int
            zipper' = zipper & moveZipper (replicate lefts Backward <> replicate ups Up)
        zipper'.cursor `shouldBe` Position{posLine = zipper.cursor.posLine - fromIntegral ups, posColumn = zipper.cursor.posColumn - fromIntegral lefts}
