{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- TODO: explicit export list for Data.Text.Lazy.Zipper
module Data.Text.Lazy.Zipper where

import Data.Int (Int64)
import Data.String (IsString (fromString))
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import GHC.Generics (Generic)
import Util
import Prelude

type Position = Word

data TextZipper = TextZipper
    { beforeCursor :: !Text
    -- ^ The text appearing before the cursor
    , afterCursor :: !Text
    -- ^ The text appearing after the cursor
    , cursor :: !Position
    -- ^ The cursor's position in the line of text, i.e. the length of 'beforeCursor'
    }
    deriving stock (Generic, Eq, Show)

-- | Modify the cursor position, updating the 'TextZipper' according to the
-- change.
moveCursor :: (Position -> Position) -> TextZipper -> TextZipper
moveCursor f t = case compare newCursor t.cursor of
    GT ->
        let (before, after)
                | absDelta > fromIntegral @Int64 maxBound = (t.afterCursor, "")
                | otherwise = Text.splitAt (fromIntegral absDelta) t.afterCursor
         in TextZipper
                { beforeCursor = t.beforeCursor <> before
                , afterCursor = after
                , cursor = t.cursor + fromIntegral (Text.length before)
                }
    LT ->
        let (before, after)
                | absDelta > fromIntegral @Int64 maxBound = ("", t.beforeCursor)
                | otherwise = splitAtEnd (fromIntegral absDelta) t.beforeCursor
         in TextZipper
                { beforeCursor = before
                , afterCursor = after <> t.afterCursor
                , cursor = t.cursor - fromIntegral (Text.length after)
                }
    EQ -> t
  where
    newCursor = f t.cursor
    absDelta
        | newCursor > t.cursor = newCursor - t.cursor
        | otherwise = t.cursor - newCursor
    splitAtEnd len t = (Text.dropEnd len t, Text.takeEnd len t)

-- | Set the position of the Cursor to a specific value. The state of the TextZipper
-- will be updated to match the new position.
setCursor :: Position -> TextZipper -> TextZipper
setCursor i = moveCursor $ const i

instance Monoid TextZipper where
    mempty = TextZipper{beforeCursor = mempty, afterCursor = mempty, cursor = 0}

instance Semigroup TextZipper where
    a <> b = a{afterCursor = a.afterCursor <> toText b}

-- | Whether the 'TextZipper' has a trailing newline. A trailing newline is
-- present if the last character of the line is a '\\n' character.
hasTrailingNewline :: TextZipper -> Bool
hasTrailingNewline TextZipper{..} = has afterCursor || Text.null afterCursor && has beforeCursor
  where
    has (Text.unsnoc -> Just (_, '\n')) = True
    has _ = False

-- | Helper function to remove the last character of the provided text iff it is
-- a trailing newline.
removeTrailingNewline :: Text -> Text
removeTrailingNewline (Text.unsnoc -> Just (t, '\n')) = t
removeTrailingNewline t = t

-- | Whether the provided 'TextZipper' is empty.
null :: TextZipper -> Bool
null TextZipper{..} = Text.null beforeCursor && Text.null afterCursor

-- | The length of the entire 'TextZipper' structure.
length :: TextZipper -> Int64
length TextZipper{..} = Text.length beforeCursor + Text.length afterCursor

-- | Convert a 'TextZipper' to 'Text'. Effectively 'beforeCursor <> afterCursor', but slightly more efficient in edge cases.
toText :: TextZipper -> Text
toText TextZipper{..}
    | Text.null afterCursor = beforeCursor
    | otherwise = beforeCursor <> afterCursor

-- | Create a 'TextZipper' from a 'Text' source, with the cursor at the end of it.
fromText :: Text -> TextZipper
fromText = flip fromParts mempty

instance IsString TextZipper where
    fromString = fromText . fromString

-- | Create a 'TextZipper' from a 'Text' source, with the cursor at the specified position.
fromTextAt :: Text -> Position -> TextZipper
fromTextAt t (max 0 -> fromIntegral -> i) = uncurry fromParts $ Text.splitAt i t

-- | Create a 'TextZipper' by concatenating two 'Text' components, with the cursor between them.
fromParts :: Text -> Text -> TextZipper
fromParts beforeCursor afterCursor =
    TextZipper
        { cursor = fromIntegral $ Text.length beforeCursor
        , ..
        }

-- | Insert 'Text' before the current Cursor position, updating its position
-- according to the provided 'Text'\'s length.
insert :: Text -> TextZipper -> TextZipper
insert t TextZipper{..} =
    TextZipper
        { beforeCursor = beforeCursor <> t
        , cursor = cursor + fromIntegral (Text.length t)
        , ..
        }

splitBefore :: TextZipper -> (TextZipper, Maybe Char)
splitBefore t@TextZipper{..} =
    case Text.unsnoc beforeCursor of
        Nothing -> (t, Nothing)
        Just (beforeCursor, c) -> (TextZipper{cursor = cursor - 1, ..}, Just c)

splitAfter :: TextZipper -> (TextZipper, Maybe Char)
splitAfter t@TextZipper{..} =
    case Text.uncons afterCursor of
        Nothing -> (t, Nothing)
        Just (c, afterCursor) -> (TextZipper{..}, Just c)

-- | Delete the first character before the cursor, if any.
deleteBefore :: TextZipper -> TextZipper
deleteBefore = fst . splitBefore

-- | Delete the first character after the cursor, if any.
deleteAfter :: TextZipper -> TextZipper
deleteAfter = fst . splitAfter

-- | Decrement the cursor.
moveBackward :: TextZipper -> TextZipper
moveBackward = moveCursor boundedPred

-- | Increment the cursor.
moveForward :: TextZipper -> TextZipper
moveForward = moveCursor boundedSucc

-- | Move the cursor to the beginning of the text.
moveStart :: TextZipper -> TextZipper
moveStart = setCursor minBound

-- | Move the cursor to the end of the text.
moveEnd :: TextZipper -> TextZipper
moveEnd = setCursor maxBound
