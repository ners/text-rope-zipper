{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- TODO: explicit export list for Data.Text.Rope.Zipper
module Data.Text.Rope.Zipper where

import Control.Arrow (first, second)
import Data.Ord (clamp)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Strict
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Zipper (TextZipper (TextZipper))
import Data.Text.Lazy.Zipper qualified as TextZipper
import Data.Text.Rope (Rope)
import Data.Text.Rope qualified as Rope
import GHC.Generics (Generic)
import GHC.Records qualified as GHC
import Prelude

-- | The Cursor is the ordered pair of the current line (0 indexed) and
-- the current position of the cursor on the line.
type Cursor = (Int, TextZipper.Cursor)

-- | A RopeZipper is similar in concept to a 'TextZipper', but tracks the
-- lines before the cursor, lines after the cursor, and the current line of the
-- cursor (as a 'TextZipper'). In essence, it is a 2D extension of 'TextZipper'.
data RopeZipper = RopeZipper
    { linesBefore :: !Rope
    -- ^ The lines before the cursor
    , currentLine :: !TextZipper
    -- ^ The line the cursor is on
    , linesAfter :: !Rope
    -- ^ The lines after the cursor
    , stickyCol :: !TextZipper.Cursor
    -- ^ The last requested cursor column. This is used to remember the
    -- column when moving between ends of lines of different lengths.
    }
    deriving stock (Generic, Eq, Show)

instance Monoid RopeZipper where
    mempty :: RopeZipper
    mempty =
        RopeZipper
            { linesBefore = mempty
            , currentLine = mempty
            , linesAfter = mempty
            , stickyCol = 0
            }

-- | '(<>)' appends the content of the second zipper after the first; the cursor
-- of the first is preserved while the cursor of the second is ignored.
instance Semigroup RopeZipper where
    (<>) :: RopeZipper -> RopeZipper -> RopeZipper
    a <> b | not (Rope.null a.linesAfter) = a{linesAfter = a.linesAfter <> toRope b}
    RopeZipper{..} <> b =
        let
            (firstLine, linesAfter) = splitFirstLine $ toRope b
         in
            RopeZipper
                { currentLine = currentLine <> TextZipper.fromText firstLine
                , ..
                }

-- | Whether the whole 'RopeZipper' structure has a trailing newline after it.
hasTrailingNewline :: RopeZipper -> Bool
hasTrailingNewline RopeZipper{..} =
    has linesAfter
        || Rope.null linesAfter
            && TextZipper.hasTrailingNewline currentLine
  where
    has (splitLastLine -> snd -> Lazy.unsnoc -> Just (_, '\n')) = True
    has _ = False

-- | Whether the whole 'RopeZipper' structure is empty.
null :: RopeZipper -> Bool
null RopeZipper{..} = Rope.null linesBefore && TextZipper.null currentLine && Rope.null linesAfter

-- | Get the current 'Cursor' position of the 'RopeZipper'.
cursor :: RopeZipper -> Cursor
cursor RopeZipper{..} = (row, col)
  where
    row = fromIntegral (Rope.lengthAsPosition linesBefore).posLine
    col = currentLine.cursor

instance GHC.HasField "cursor" RopeZipper Cursor where
    getField = cursor

-- | Move the cursor relative to its current position.
moveCursor :: (Cursor -> Cursor) -> RopeZipper -> RopeZipper
moveCursor f r
    -- for a positive change where the current line has a trailing newline OR
    -- there are lines after the current line
    | (dy > 0)
        && (TextZipper.hasTrailingNewline r.currentLine || not (Rope.null r.linesAfter)) =
        let (before, currentLine, linesAfter) = splitAtLine (fromIntegral $ dy - 1) r.linesAfter
         in RopeZipper
                { linesBefore = r.linesBefore <> currentLineAsRope <> before
                , stickyCol =
                    if dx /= 0
                        then min currentLine.cursor newX
                        else r.stickyCol
                , ..
                }
    -- for a negative change that puts the row of the cursor at the start
    | dy < 0 && newY == 0 =
        let ( flip TextZipper.fromTextAt newX ->
                    moveBackFromNewline -> currentLine
                , linesAfter
                ) = splitFirstLine $ r.linesBefore <> currentLineAsRope <> r.linesAfter
         in RopeZipper
                { linesBefore = mempty
                , currentLine
                , stickyCol =
                    if dx /= 0
                        then min currentLine.cursor newX
                        else r.stickyCol
                , ..
                }
    -- for a negative change that doesn't put the row of the cursor at the start
    | dy < 0 && not (Rope.null r.linesBefore) =
        let (linesBefore, currentLine, after) =
                splitAtLine
                    ((Rope.lengthAsPosition r.linesBefore).posLine - fromIntegral (negate dy))
                    r.linesBefore
         in RopeZipper
                { linesAfter = after <> currentLineAsRope <> r.linesAfter
                , stickyCol =
                    if dx /= 0
                        then min currentLine.cursor newX
                        else r.stickyCol
                , ..
                }
    -- in any other circumstance, just move the cursor within the line and reset
    -- the current cursor pos
    | otherwise =
        let currentLine = moveBackFromNewline $ TextZipper.moveCursor (+ dx) r.currentLine
         in r
                { currentLine
                , stickyCol =
                    if (r.stickyCol > currentLine.cursor && dx > 0)
                        || (dy /= 0 && dx == 0)
                        then r.stickyCol
                        else currentLine.cursor
                }
  where
    (oldY, oldX) = r.cursor
    (max 0 -> newY, max 0 -> newX) = f r.cursor
    (dy, dx) = (newY - oldY, newX - oldX)

    currentLineAsRope = Rope.fromText . Lazy.toStrict . TextZipper.toText $ r.currentLine
    splitAtLine :: Word -> Rope -> (Rope, TextZipper, Rope)
    splitAtLine n rope =
        let (before, after) = Rope.splitAtLine (clamp (0, (Rope.lengthAsPosition rope).posLine) n) rope
            (current, after') = splitFirstLine after
            stickyCol = if dx /= 0 then newX else r.stickyCol
         in (before, moveBackFromNewline $ TextZipper.fromTextAt current stickyCol, after')

-- | Move the cursor to the given absolute position.
setCursor :: Cursor -> RopeZipper -> RopeZipper
setCursor c = moveCursor (const c)

lines :: RopeZipper -> [Text]
lines = Rope.lines . toRope

lengthInLines :: RopeZipper -> Word
lengthInLines r@RopeZipper{..} = rowsBefore + currentLineLength + rowsAfter
  where
    rowsBefore = fromIntegral . fst . cursor $ r
    currentLineLength = if TextZipper.null currentLine then 0 else 1
    rowsAfter = Rope.lengthInLines linesAfter

toRope :: RopeZipper -> Rope
toRope RopeZipper{..} =
    mconcat
        [ linesBefore
        , Rope.fromText $ Lazy.toStrict $ TextZipper.toText currentLine
        , linesAfter
        ]

toText :: RopeZipper -> Text
toText = Rope.toText . toRope

fromParts :: Rope -> Rope -> RopeZipper
fromParts r1 r2 = RopeZipper{..}
  where
    (linesBefore, beforeCursor) = splitLastLine r1
    (afterCursor, linesAfter) = splitFirstLine r2
    currentLine = TextZipper.fromParts beforeCursor afterCursor
    stickyCol = currentLine.cursor

fromRope :: Rope -> RopeZipper
fromRope = flip fromParts mempty

fromText :: Text -> RopeZipper
fromText = fromRope . Rope.fromText

instance IsString RopeZipper where
    fromString = fromText . fromString

splitFirstLine :: Rope -> (Lazy.Text, Rope)
splitFirstLine r = (Lazy.fromStrict $ Rope.toText firstLine, linesAfter)
  where
    (firstLine, linesAfter) = Rope.splitAtLine 1 r

splitLastLine :: Rope -> (Rope, Lazy.Text)
splitLastLine r = (linesBefore, Lazy.fromStrict $ Rope.toText lastLine)
  where
    (linesBefore, lastLine) = Rope.splitAtLine (Rope.lengthAsPosition r).posLine r

split2ndLastLine :: Rope -> (Rope, Lazy.Text)
split2ndLastLine r
    | numLines < 1 = ("", Lazy.fromStrict $ Rope.toText r)
    | otherwise = (linesBefore, Lazy.fromStrict $ Rope.toText lastLine)
  where
    numLines = (Rope.lengthAsPosition r).posLine
    (linesBefore, lastLine) = Rope.splitAtLine (numLines - 1) r

insertRope :: Rope -> RopeZipper -> RopeZipper
insertRope rope@(Rope.lengthAsPosition -> Rope.Position{posLine = 0}) r =
    r{currentLine, stickyCol = currentLine.cursor}
  where
    currentLine = TextZipper.insert (Lazy.fromStrict $ Rope.toText rope) r.currentLine
insertRope (fromRope -> t) r =
    r{linesBefore, currentLine, stickyCol = currentLine.cursor}
  where
    linesBefore =
        r.linesBefore
            <> Rope.fromText (Lazy.toStrict r.currentLine.beforeCursor)
            <> t.linesBefore
    currentLine = t.currentLine <> TextZipper.fromText r.currentLine.afterCursor

insertText :: Text -> RopeZipper -> RopeZipper
insertText = insertRope . Rope.fromText

insertChar :: Char -> RopeZipper -> RopeZipper
insertChar = insertText . Strict.singleton

deleteBefore :: RopeZipper -> RopeZipper
deleteBefore r@RopeZipper{currentLine = TextZipper{beforeCursor = Lazy.null -> False}} =
    r{currentLine, stickyCol = currentLine.cursor}
  where
    currentLine = TextZipper.deleteBefore r.currentLine
deleteBefore r = RopeZipper{linesAfter = r.linesAfter, ..}
  where
    (linesBefore, TextZipper.removeTrailingNewline -> beforeCursor) = split2ndLastLine r.linesBefore
    currentLine = TextZipper.fromParts beforeCursor r.currentLine.afterCursor
    stickyCol = currentLine.cursor

deleteAfter :: RopeZipper -> RopeZipper
deleteAfter r@RopeZipper{currentLine = TextZipper{afterCursor = "\n"}} = RopeZipper{..}
  where
    linesBefore = r.linesBefore
    (afterCursor, linesAfter) = splitFirstLine r.linesAfter
    currentLine = TextZipper.fromParts r.currentLine.beforeCursor afterCursor
    stickyCol = currentLine.cursor
deleteAfter RopeZipper{..} = RopeZipper{currentLine = TextZipper.deleteAfter currentLine, ..}

-- | Move the cursor to the previous character of the current row, if there is one. Does not change rows.
moveBackward :: RopeZipper -> RopeZipper
moveBackward = moveCursor $ second pred

-- | Move the cursor to the next character of the current row, if there is one. Does not change rows.
moveForward :: RopeZipper -> RopeZipper
moveForward = moveCursor $ second succ

-- | Move the cursor to the previous row, trying to preserve the column.
moveUp :: RopeZipper -> RopeZipper
moveUp = moveCursor $ first pred

-- | Move the cursor to the next row, trying to preserve the column.
moveDown :: RopeZipper -> RopeZipper
moveDown = moveCursor $ first succ

-- | Move the cursor to the start of the current line.
moveToLineStart :: RopeZipper -> RopeZipper
moveToLineStart = moveCursor $ second $ const 0

-- | Move the cursor to the end of the current line.
moveToLineEnd :: RopeZipper -> RopeZipper
moveToLineEnd = moveCursor $ second $ const maxBound

-- | Move the cursor to the start of the current line.
moveToStart :: RopeZipper -> RopeZipper
moveToStart = moveCursor $ first $ const 0

-- | Move the cursor to the end of the current line.
moveToEnd :: RopeZipper -> RopeZipper
moveToEnd = moveCursor $ first $ const maxBound

-- If the cursor is after the final character and the final character is a newline,
-- move backwards once.
moveBackFromNewline :: TextZipper -> TextZipper
moveBackFromNewline t
    | TextZipper.hasTrailingNewline t && Lazy.null t.afterCursor =
        TextZipper.moveBackward t
    | otherwise = t
