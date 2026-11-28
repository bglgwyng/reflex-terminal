{-# LANGUAGE LambdaCase #-}

-- | Basic xterm input parser.
module System.Console.Input (
  Key (..),
  Modifier (..),
  KeyPress (..),
  keyPress,
)
where

import Control.Applicative
import Data.Attoparsec.ByteString qualified as A
import Data.Bits
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.Word

data Key
  = KChar Char
  | KEnter
  | KTab
  | KBackspace
  | KEsc
  | KUp
  | KDown
  | KLeft
  | KRight
  | KHome
  | KEnd
  | KPageUp
  | KPageDown
  | KInsert
  | KDelete
  | KFun Int
  | -- | Unrecognized escape sequence (bytes after @ESC@)
    KUnknown BS.ByteString
  deriving stock (Eq, Ord, Show)

data Modifier = Shift | Alt | Ctrl
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data KeyPress = KeyPress Key [Modifier]
  deriving stock (Eq, Ord, Show)

plain :: Key -> KeyPress
plain k = KeyPress k []

withMod :: Modifier -> KeyPress -> KeyPress
withMod m (KeyPress k ms) = KeyPress k (m : ms)

{- | Parse a single key press. Never fails on non-empty input.

NOTE: A lone @ESC@ waits for the next byte, since it can't be distinguished
from the start of an escape sequence without a timeout.
-}
keyPress :: A.Parser KeyPress
keyPress =
  A.anyWord8 >>= \case
    0x1b -> escape
    b -> byte b

byte :: Word8 -> A.Parser KeyPress
byte = \case
  0x0d -> pure $ plain KEnter
  0x0a -> pure $ plain KEnter
  0x09 -> pure $ plain KTab
  0x7f -> pure $ plain KBackspace
  0x08 -> pure $ plain KBackspace
  0x00 -> pure $ KeyPress (KChar ' ') [Ctrl]
  b
    | b < 0x20 -> pure $ KeyPress (KChar (chr (fromIntegral b + 0x60))) [Ctrl]
    | b < 0x80 -> pure $ plain (KChar (chr (fromIntegral b)))
    | otherwise -> plain . KChar <$> utf8 b

utf8 :: Word8 -> A.Parser Char
utf8 b0
  | b0 .&. 0xe0 == 0xc0 = go 1 (b0 .&. 0x1f)
  | b0 .&. 0xf0 == 0xe0 = go 2 (b0 .&. 0x0f)
  | b0 .&. 0xf8 == 0xf0 = go 3 (b0 .&. 0x07)
  | otherwise = pure replacement
 where
  replacement = '\xFFFD'
  go :: Int -> Word8 -> A.Parser Char
  go n lead =
    ( do
        rest <- A.count n (A.satisfy \b -> b .&. 0xc0 == 0x80)
        let cp = foldl (\acc b -> acc `shiftL` 6 .|. fromIntegral (b .&. 0x3f)) (fromIntegral lead) rest
        pure if cp > 0x10ffff then replacement else chr cp
    )
      <|> pure replacement

escape :: A.Parser KeyPress
escape =
  (A.word8 0x5b *> csi) -- ESC [
    <|> (A.word8 0x4f *> ss3) -- ESC O
    <|> (A.satisfy (/= 0x1b) >>= fmap (withMod Alt) . byte) -- ESC <key> = Alt+<key>
    <|> pure (plain KEsc)

-- | @ESC O <final>@: F1-F4 and application-mode cursor keys
ss3 :: A.Parser KeyPress
ss3 =
  A.anyWord8 >>= \b -> maybe empty (pure . plain) (finalKey b)

-- | @ESC [ <params> <intermediates> <final>@
csi :: A.Parser KeyPress
csi = do
  (raw, (params, final)) <- A.match do
    paramBytes <- A.takeWhile \b -> b >= 0x30 && b <= 0x3f
    _ <- A.takeWhile \b -> b >= 0x20 && b <= 0x2f
    final <- A.satisfy \b -> b >= 0x40 && b <= 0x7e
    pure (map readNum (BS.split 0x3b paramBytes), final)
  let mods = case params of
        _ : m : _ -> modifiers m
        _ -> []
      unknown = plain (KUnknown (BS.cons 0x5b raw))
  pure case final of
    0x7e -- '~'
      | p : _ <- params, Just k <- tildeKey p -> KeyPress k mods
      | otherwise -> unknown
    0x5a -> KeyPress KTab [Shift] -- 'Z'
    _
      | Just k <- finalKey final -> KeyPress k mods
      | otherwise -> unknown

finalKey :: Word8 -> Maybe Key
finalKey = \case
  0x41 -> Just KUp -- A
  0x42 -> Just KDown -- B
  0x43 -> Just KRight -- C
  0x44 -> Just KLeft -- D
  0x48 -> Just KHome -- H
  0x46 -> Just KEnd -- F
  0x50 -> Just (KFun 1) -- P
  0x51 -> Just (KFun 2) -- Q
  0x52 -> Just (KFun 3) -- R
  0x53 -> Just (KFun 4) -- S
  _ -> Nothing

tildeKey :: Int -> Maybe Key
tildeKey = \case
  1 -> Just KHome
  2 -> Just KInsert
  3 -> Just KDelete
  4 -> Just KEnd
  5 -> Just KPageUp
  6 -> Just KPageDown
  7 -> Just KHome
  8 -> Just KEnd
  n
    | n >= 11 && n <= 15 -> Just (KFun (n - 10))
    | n >= 17 && n <= 21 -> Just (KFun (n - 11))
    | n == 23 || n == 24 -> Just (KFun (n - 12))
    | otherwise -> Nothing

-- | xterm modifier parameter: 1 + (Shift=1 | Alt=2 | Ctrl=4)
modifiers :: Int -> [Modifier]
modifiers p =
  [m | (i, m) <- [(0, Shift), (1, Alt), (2, Ctrl)], testBit (p - 1) i]

readNum :: BS.ByteString -> Int
readNum bs
  | BS.null bs = 1
  | otherwise = BS.foldl' (\acc b -> acc * 10 + fromIntegral (b - 0x30)) 0 (BS.filter (\b -> b >= 0x30 && b <= 0x39) bs)
