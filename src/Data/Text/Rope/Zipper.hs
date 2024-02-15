{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Text.Rope.Zipper
    ( RopeZipper (..)
    , Position (..)
    , null
    , cursor
    , moveCursor
    , setCursor
    , lines
    , lengthInLines
    , toRope
    , toText
    , fromParts
    , fromRope
    , fromText
    , splitFirstLine
    , splitLastLine
    , insertRope
    , insertText
    , insertChar
    , deleteBefore
    , deleteAfter
    , moveForward
    , moveBackward
    , moveUp
    , moveDown
    , moveToLineStart
    , moveToLineEnd
    , moveToFirstLine
    , moveToLastLine
    )
where

import Data.Ord (clamp)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Strict
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Zipper (TextZipper (TextZipper))
import Data.Text.Lazy.Zipper qualified as TextZipper
import Data.Text.Rope (Position (..), Rope)
import Data.Text.Rope qualified as Rope
import GHC.Generics (Generic)
import GHC.Records qualified as GHC
import Util
import Prelude hiding (lines, null)

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
    , stickyCol :: !TextZipper.Position
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

-- | Whether the whole 'RopeZipper' structure is empty.
null :: RopeZipper -> Bool
null RopeZipper{..} = Rope.null linesBefore && TextZipper.null currentLine && Rope.null linesAfter

-- | Get the current cursor position of the 'RopeZipper'.
cursor :: RopeZipper -> Position
cursor RopeZipper{..} =
    Position
        { posLine = (Rope.lengthAsPosition linesBefore).posLine
        , posColumn = currentLine.cursor
        }

instance GHC.HasField "cursor" RopeZipper Position where
    getField = cursor

-- | Move the cursor relative to its current position.
moveCursor :: (Position -> Position) -> RopeZipper -> RopeZipper
moveCursor f r
    -- for a positive change where the current line has a trailing newline OR
    -- there are lines after the current line
    | (newY > oldY)
        && (TextZipper.hasTrailingNewline r.currentLine || not (Rope.null r.linesAfter)) =
        let (before, currentLine, linesAfter) = splitAtLine (absDy - 1) r.linesAfter
         in RopeZipper
                { linesBefore = r.linesBefore <> currentLineAsRope <> before
                , stickyCol = withStickyCol $ min currentLine.cursor
                , ..
                }
    -- for a negative change that puts the line of the cursor at the start
    | newY < oldY && newY == 0 =
        let ( flip TextZipper.fromTextAt (if absDx /= 0 then newX else r.stickyCol) ->
                    moveBackFromNewline -> currentLine
                , linesAfter
                ) = splitFirstLine $ r.linesBefore <> currentLineAsRope <> r.linesAfter
         in RopeZipper
                { linesBefore = mempty
                , currentLine
                , stickyCol = withStickyCol $ min currentLine.cursor
                , ..
                }
    -- for a negative change that doesn't put the line of the cursor at the start
    | newY < oldY && not (Rope.null r.linesBefore) =
        let (linesBefore, currentLine, after) =
                splitAtLine
                    ((Rope.lengthAsPosition r.linesBefore).posLine - absDy)
                    r.linesBefore
         in RopeZipper
                { linesAfter = after <> currentLineAsRope <> r.linesAfter
                , stickyCol = withStickyCol $ min currentLine.cursor
                , ..
                }
    -- in any other circumstance, just move the cursor within the line and reset
    -- the current cursor pos
    | otherwise =
        let currentLine = moveBackFromNewline $ TextZipper.setCursor newX r.currentLine
         in r
                { currentLine
                , stickyCol =
                    if (r.stickyCol > currentLine.cursor && absDx > 0)
                        || (absDy /= 0 && absDx == 0)
                        then r.stickyCol
                        else currentLine.cursor
                }
  where
    Position{posLine = oldY, posColumn = oldX} = r.cursor
    Position{posLine = newY, posColumn = newX} = f r.cursor
    absDy = absDelta newY oldY
    absDx = absDelta newX oldX
    withStickyCol f = if absDx == 0 then r.stickyCol else f newX

    currentLineAsRope = Rope.fromText . Lazy.toStrict . TextZipper.toText $ r.currentLine
    splitAtLine :: Word -> Rope -> (Rope, TextZipper, Rope)
    splitAtLine n rope =
        let (before, after) = Rope.splitAtLine (clamp (0, (Rope.lengthAsPosition rope).posLine) n) rope
            (current, after') = splitFirstLine after
            stickyCol = if absDx /= 0 then newX else r.stickyCol
         in (before, moveBackFromNewline $ TextZipper.fromTextAt current stickyCol, after')

-- | Move the cursor to the given absolute position.
setCursor :: Position -> RopeZipper -> RopeZipper
setCursor c = moveCursor (const c)

lines :: RopeZipper -> [Text]
lines = Rope.lines . toRope

lengthInLines :: RopeZipper -> Word
lengthInLines r@RopeZipper{..} = before + current + after
  where
    before = posLine . cursor $ r
    current = if TextZipper.null currentLine then 0 else 1
    after = Rope.lengthInLines linesAfter

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

-- | Move the cursor to the previous character of the current row, if there is one. Does not change lines.
moveBackward :: RopeZipper -> RopeZipper
moveBackward = moveCursor $ \c -> c{posColumn = boundedPred c.posColumn}

-- | Move the cursor to the next character of the current line, if there is one. Does not change lines.
moveForward :: RopeZipper -> RopeZipper
moveForward = moveCursor $ \c -> c{posColumn = boundedSucc c.posColumn}

-- | Move the cursor to the previous line, trying to preserve the column.
moveUp :: RopeZipper -> RopeZipper
moveUp = moveCursor $ \c -> c{posLine = boundedPred c.posLine}

-- | Move the cursor to the next line, trying to preserve the column.
moveDown :: RopeZipper -> RopeZipper
moveDown = moveCursor $ \c -> c{posLine = boundedSucc c.posLine}

-- | Move the cursor to the start of the current line.
moveToLineStart :: RopeZipper -> RopeZipper
moveToLineStart = moveCursor $ \c -> c{posColumn = minBound}

-- | Move the cursor to the end of the current line.
moveToLineEnd :: RopeZipper -> RopeZipper
moveToLineEnd = moveCursor $ \c -> c{posColumn = maxBound}

-- | Move the cursor to the first line, trying to preserve the column.
moveToFirstLine :: RopeZipper -> RopeZipper
moveToFirstLine = moveCursor $ \c -> c{posLine = minBound}

-- | Move the cursor to the last line, trying to preserve the column.
moveToLastLine :: RopeZipper -> RopeZipper
moveToLastLine = moveCursor $ \c -> c{posLine = maxBound}

-- If the cursor is after the final character and the final character is a newline,
-- move backwards once.
moveBackFromNewline :: TextZipper -> TextZipper
moveBackFromNewline t
    | TextZipper.hasTrailingNewline t && Lazy.null t.afterCursor =
        TextZipper.moveBackward t
    | otherwise = t
