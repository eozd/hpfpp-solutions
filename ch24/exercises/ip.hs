module IP where

import           Control.Applicative ((<|>))
import           Data.Bits
import           Data.Char
import           Data.List           (elemIndex, intercalate)
import           Data.Maybe          (fromMaybe)
import           Data.Word
import           Debug.Trace
import           Numeric             (readHex, showHex)
import           Text.Trifecta

newtype IPv4Addr =
  IPv4Addr Word32
  deriving (Eq, Ord)

data IPv6Addr =
  IPv6Addr Word64
           Word64
  deriving (Eq, Ord)

instance Show IPv6Addr where
  show (IPv6Addr w1 w2) =
    let wList = word64ToWord16List w1 ++ word64ToWord16List w2
     in intercalate ":" $ map (flip showHex "") wList

instance Show IPv4Addr where
  show (IPv4Addr w) =
    let bytes = word32ToByteList w
     in intercalate "." $ map show bytes

word16ListToWord64 :: [Word16] -> Word64
word16ListToWord64 (w1:w2:w3:w4:[]) =
  (fromIntegral w1 `shiftL` 48) + (fromIntegral w2 `shiftL` 32) +
  (fromIntegral w3 `shiftL` 16) +
  (fromIntegral w4)

word64ToWord16List :: Word64 -> [Word16]
word64ToWord16List w =
  map
    fromIntegral
    [ (w .&. 0xFFFF000000000000) `shiftR` 48
    , (w .&. 0x0000FFFF00000000) `shiftR` 32
    , (w .&. 0x00000000FFFF0000) `shiftR` 16
    , w .&. 0x000000000000FFFF
    ]

word32ToByteList :: Word32 -> [Word8]
word32ToByteList w =
  map
    fromIntegral
    [ (w .&. 0xFF000000) `shiftR` 24
    , (w .&. 0x00FF0000) `shiftR` 16
    , (w .&. 0x0000FF00) `shiftR` 8
    , w .&. 0x000000FF
    ]

byteListToWord32 :: [Word8] -> Word32
byteListToWord32 (w1:w2:w3:w4:[]) =
  (fromIntegral w1 `shiftL` 24) + (fromIntegral w2 `shiftL` 16) +
  (fromIntegral w3 `shiftL` 8) +
  fromIntegral w4

parseIPv4Word :: Parser Word8
parseIPv4Word = do
  numStr <- choice [try (count 3 digit), try (count 2 digit), count 1 digit]
  let num = read numStr :: Word8
  return num

parseIPv4 :: Parser IPv4Addr
parseIPv4 = do
  bHead <- parseIPv4Word
  bRest <- count 3 (char '.' >> parseIPv4Word)
  let ipDecimal = byteListToWord32 (bHead : bRest)
  return $ IPv4Addr ipDecimal

parseHexDigit :: Parser Char
parseHexDigit =
  satisfy
    (\c ->
       let n = ord c
        in (n >= ord '0' && n <= ord '9') ||
           (n >= ord 'a' && n <= ord 'f') || (n >= ord 'A' && n <= ord 'F'))

parseIPv6Word :: Parser String
parseIPv6Word = do
  numStr <-
    choice
      [ try (count 4 parseHexDigit)
      , try (count 3 parseHexDigit)
      , try (count 2 parseHexDigit)
      , count 1 parseHexDigit
      ]
  return numStr

parseIPv6 :: Parser IPv6Addr
parseIPv6 = do
  wHead <- try parseIPv6Word <|> string ":"
  wList <-
    manyTill
      (try (char ':' >> parseIPv6Word) <|> (string ":"))
      (eof <|> (satisfy isSpace >> return ()))
  let allCombined = wHead : wList
      ws =
        if drop (length allCombined - 2) allCombined == [":", ":"]
          then take (length allCombined - 1) allCombined
          else allCombined
      numAbbr = length . filter (== ":") $ ws
  case numAbbr of
    0 -> return $ mkIPv6 ws
    1 ->
      let abbrIdx = fromMaybe (-1) $ elemIndex ":" ws
          numToInsert = 8 - (length ws) + 1
          wsFull =
            take abbrIdx ws ++
            replicate numToInsert "0" ++ drop (abbrIdx + 1) ws
       in return $ mkIPv6 wsFull
    otherwise -> fail "Multiple abbreviation operators (::) are not allowed"
  where
    mkIPv6 ws =
      let ws16List = map (fst . head . readHex) ws
          ipWord1 = word16ListToWord64 (take 4 ws16List)
          ipWord2 = word16ListToWord64 (drop 4 ws16List)
       in IPv6Addr ipWord1 ipWord2

toIPv6 :: IPv4Addr -> IPv6Addr
toIPv6 (IPv4Addr w) = IPv6Addr 0 (fromIntegral w)

toIPv4 :: IPv6Addr -> Maybe IPv4Addr
toIPv4 (IPv6Addr w1 w2)
  | w1 /= 0 = Nothing
  | w2 >= (2 ^ 32) = Nothing
  | otherwise = Just $ IPv4Addr (fromIntegral w2)

main :: IO ()
main = do
  print $ parseString parseIPv4 mempty "172.16.254.1"
  print $ parseString parseIPv4 mempty "204.120.0.15"
  print $
    parseString parseIPv6 mempty "FE80:0000:0000:0000:0202:B3FF:FE1E:8329 "
  print $ parseString parseIPv6 mempty "FE80::0202:B3FF:FE1E:8329"
  print $ parseString parseIPv6 mempty "2001:DB8::8:800:200C:417A"
  print $ parseString parseIPv6 mempty "::1"
  print $ parseString parseIPv6 mempty "1::"
  print $ parseString parseIPv6 mempty "1::1"
  print $ parseString parseIPv6 mempty "::"
  print $ parseString parseIPv6 mempty "2001:DB8:AC10:FE01::"
