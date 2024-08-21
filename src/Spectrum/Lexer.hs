module Spectrum.Lexer
  ( Style(..)
  , LexerError(..)
  , styleToANSI
  , colorNameMapping
  , tagToCode
  , fgFromName
  , bgFromName
  , parseStyle
  , parseStyleString
  , parseHexColor
  ) where

import Text.Parsec hiding ((<|>))
import Text.Parsec.String (Parser)
import Control.Applicative ((<|>))
import qualified Data.Map as Map
import Data.Char (toLower)
import Text.Printf (printf)
import Control.Monad (liftM2)
import Data.Functor (($>))
import Text.Read (readMaybe)

data Style = Bold | Dim | Italic | Underline | Blink | RapidBlink | Inverse | Hidden | Strikethrough
  deriving (Show, Eq, Enum)

data LexerError
  = InvalidTag String
  | InvalidStyleName String
  | InvalidColorName String
  | InvalidHexColor String
  | InvalidRgbColor String
  | InvalidHslColor String
  | InvalidPercentage String
  | InvalidQualifier String
  deriving (Show)

styleToANSI :: Style -> String
styleToANSI style = case style of
  Bold -> "1"
  Dim -> "2"
  Italic -> "3"
  Underline -> "4"
  Blink -> "5"
  RapidBlink -> "5"
  Inverse -> "7"
  Hidden -> "8"
  Strikethrough -> "9"

-- Extensive color name mapping (truncated for brevity)
colorNameMapping :: Map.Map String String
colorNameMapping = Map.fromList
  [ ("black", "0")
  , ("maroon", "1")
  , ("green", "2")
  , ("olive", "3")
  , ("navy", "4")
  , ("purple", "5")
  , ("teal", "6")
  , ("silver", "7")
  , ("grey", "8")
  , ("red", "9")
  , ("lime", "10")
  , ("yellow", "11")
  , ("blue", "12")
  , ("fuchsia", "13")
  , ("aqua", "14")
  , ("white", "15")
  , ("grey-0", "16")
  , ("navy-blue", "17")
  , ("dark-blue", "18")
  , ("blue-3a", "19")
  , ("blue-3b", "20")
  , ("blue-1", "21")
  , ("dark-green", "22")
  , ("deep-sky-blue-4a", "23")
  , ("deep-sky-blue-4b", "24")
  , ("deep-sky-blue-4c", "25")
  , ("dodger-blue-3", "26")
  , ("dodger-blue-2", "27")
  , ("green-4", "28")
  , ("spring-green-4", "29")
  , ("turquoise-4", "30")
  , ("deep-sky-blue-3a", "31")
  , ("deep-sky-blue-3b", "32")
  , ("dodger-blue-1", "33")
  , ("green-3a", "34")
  , ("spring-green-3a", "35")
  , ("dark-cyan", "36")
  , ("light-sea-green", "37")
  , ("deep-sky-blue-2", "38")
  , ("deep-sky-blue-1", "39")
  , ("green-3b", "40")
  , ("spring-green-3b", "41")
  , ("spring-green-2a", "42")
  , ("cyan-3", "43")
  , ("dark-turquoise", "44")
  , ("turquoise-2", "45")
  , ("green-1", "46")
  , ("spring-green-2b", "47")
  , ("spring-green-1", "48")
  , ("medium-spring-green", "49")
  , ("cyan-2", "50")
  , ("cyan-1", "51")
  , ("dark-red-1", "52")
  , ("deep-pink-4a", "53")
  , ("purple-4a", "54")
  , ("purple-4b", "55")
  , ("purple-3", "56")
  , ("blue-violet", "57")
  , ("orange-4a", "58")
  , ("grey-37", "59")
  , ("medium-purple-4", "60")
  , ("slate-blue-3a", "61")
  , ("slate-blue-3b", "62")
  , ("royal-blue-1", "63")
  , ("chartreuse-4", "64")
  , ("dark-sea-green-4a", "65")
  , ("pale-turquoise-4", "66")
  , ("steel-blue", "67")
  , ("steel-blue-3", "68")
  , ("cornflower-blue", "69")
  , ("chartreuse-3a", "70")
  , ("dark-sea-green-4b", "71")
  , ("cadet-blue-2", "72")
  , ("cadet-blue-1", "73")
  , ("sky-blue-3", "74")
  , ("steel-blue-1a", "75")
  , ("chartreuse-3b", "76")
  , ("pale-green-3a", "77")
  , ("sea-green-3", "78")
  , ("aquamarine-3", "79")
  , ("medium-turquoise", "80")
  , ("steel-blue-1b", "81")
  , ("chartreuse-2a", "82")
  , ("sea-green-2", "83")
  , ("sea-green-1a", "84")
  , ("sea-green-1b", "85")
  , ("aquamarine-1a", "86")
  , ("dark-slate-gray-2", "87")
  , ("dark-red-2", "88")
  , ("deep-pink-4b", "89")
  , ("dark-magenta-1", "90")
  , ("dark-magenta-2", "91")
  , ("dark-violet-1a", "92")
  , ("purple-1a", "93")
  , ("orange-4b", "94")
  , ("light-pink-4", "95")
  , ("plum-4", "96")
  , ("medium-purple-3a", "97")
  , ("medium-purple-3b", "98")
  , ("slate-blue-1", "99")
  , ("yellow-4a", "100")
  , ("wheat-4", "101")
  , ("grey-53", "102")
  , ("light-slate-grey", "103")
  , ("medium-purple", "104")
  , ("light-slate-blue", "105")
  , ("yellow-4b", "106")
  , ("dark-olive-green-3a", "107")
  , ("dark-green-sea", "108")
  , ("light-sky-blue-3a", "109")
  , ("light-sky-blue-3b", "110")
  , ("sky-blue-2", "111")
  , ("chartreuse-2b", "112")
  , ("dark-olive-green-3b", "113")
  , ("pale-green-3b", "114")
  , ("dark-sea-green-3a", "115")
  , ("dark-slate-gray-3", "116")
  , ("sky-blue-1", "117")
  , ("chartreuse-1", "118")
  , ("light-green-2", "119")
  , ("light-green-3", "120")
  , ("pale-green-1a", "121")
  , ("aquamarine-1b", "122")
  , ("dark-slate-gray-1", "123")
  , ("red-3a", "124")
  , ("deep-pink-4c", "125")
  , ("medium-violet-red", "126")
  , ("magenta-3a", "127")
  , ("dark-violet-1b", "128")
  , ("purple-1b", "129")
  , ("dark-orange-3a", "130")
  , ("indian-red-1a", "131")
  , ("hot-pink-3a", "132")
  , ("medium-orchid-3", "133")
  , ("medium-orchid", "134")
  , ("medium-purple-2a", "135")
  , ("dark-goldenrod", "136")
  , ("light-salmon-3a", "137")
  , ("rosy-brown", "138")
  , ("grey-63", "139")
  , ("medium-purple-2b", "140")
  , ("medium-purple-1", "141")
  , ("gold-3a", "142")
  , ("dark-khaki", "143")
  , ("navajo-white-3", "144")
  , ("grey-69", "145")
  , ("light-steel-blue-3", "146")
  , ("light-steel-blue", "147")
  , ("yellow-3a", "148")
  , ("dark-olive-green-3", "149")
  , ("dark-sea-green-3b", "150")
  , ("dark-sea-green-2", "151")
  , ("light-cyan-3", "152")
  , ("light-sky-blue-1", "153")
  , ("green-yellow", "154")
  , ("dark-olive-green-2", "155")
  , ("pale-green-1b", "156")
  , ("dark-sea-green-5b", "157")
  , ("dark-sea-green-5a", "158")
  , ("pale-turquoise-1", "159")
  , ("red-3b", "160")
  , ("deep-pink-3a", "161")
  , ("deep-pink-3b", "162")
  , ("magenta-3b", "163")
  , ("magenta-3c", "164")
  , ("magenta-2a", "165")
  , ("dark-orange-3b", "166")
  , ("indian-red-1b", "167")
  , ("hot-pink-3b", "168")
  , ("hot-pink-2", "169")
  , ("orchid", "170")
  , ("medium-orchid-1a", "171")
  , ("orange-3", "172")
  , ("light-salmon-3b", "173")
  , ("light-pink-3", "174")
  , ("pink-3", "175")
  , ("plum-3", "176")
  , ("violet", "177")
  , ("gold-3b", "178")
  , ("light-goldenrod-3", "179")
  , ("tan", "180")
  , ("misty-rose-3", "181")
  , ("thistle-3", "182")
  , ("plum-2", "183")
  , ("yellow-3b", "184")
  , ("khaki-3", "185")
  , ("light-goldenrod-2a", "186")
  , ("light-yellow-3", "187")
  , ("grey-84", "188")
  , ("light-steel-blue-1", "189")
  , ("yellow-2", "190")
  , ("dark-olive-green-1a", "191")
  , ("dark-olive-green-1b", "192")
  , ("dark-sea-green-1", "193")
  , ("honeydew-2", "194")
  , ("light-cyan-1", "195")
  , ("red-1", "196")
  , ("deep-pink-2", "197")
  , ("deep-pink-1a", "198")
  , ("deep-pink-1b", "199")
  , ("magenta-2b", "200")
  , ("magenta-1", "201")
  , ("orange-red-1", "202")
  , ("indian-red-1c", "203")
  , ("indian-red-1d", "204")
  , ("hot-pink-1a", "205")
  , ("hot-pink-1b", "206")
  , ("medium-orchid-1b", "207")
  , ("dark-orange", "208")
  , ("salmon-1", "209")
  , ("light-coral", "210")
  , ("pale-violet-red-1", "211")
  , ("orchid-2", "212")
  , ("orchid-1", "213")
  , ("orange-1", "214")
  , ("sandy-brown", "215")
  , ("light-salmon-1", "216")
  , ("light-pink-1", "217")
  , ("pink-1", "218")
  , ("plum-1", "219")
  , ("gold-1", "220")
  , ("light-goldenrod-2b", "221")
  , ("light-goldenrod-2c", "222")
  , ("navajo-white-1", "223")
  , ("misty-rose1", "224")
  , ("thistle-1", "225")
  , ("yellow-1", "226")
  , ("light-goldenrod-1", "227")
  , ("khaki-1", "228")
  , ("wheat-1", "229")
  , ("cornsilk-1", "230")
  , ("grey-100", "231")
  , ("grey-3", "232")
  , ("grey-7", "233")
  , ("grey-11", "234")
  , ("grey-15", "235")
  , ("grey-19", "236")
  , ("grey-23", "237")
  , ("grey-27", "238")
  , ("grey-30", "239")
  , ("grey-35", "240")
  , ("grey-39", "241")
  , ("grey-42", "242")
  , ("grey-46", "243")
  , ("grey-50", "244")
  , ("grey-54", "245")
  , ("grey-58", "246")
  , ("grey-62", "247")
  , ("grey-66", "248")
  , ("grey-70", "249")
  , ("grey-74", "250")
  , ("grey-78", "251")
  , ("grey-82", "252")
  , ("grey-85", "253")
  , ("grey-89", "254")
  , ("grey-93", "255")
  ]

-- Helper function for qualified colors
qualifyColor :: Maybe String -> String -> String
qualifyColor (Just "bg:") c = "48;" ++ c
qualifyColor _ c            = "38;" ++ c

fgFromName, bgFromName :: String -> Either LexerError String
fgFromName name = case Map.lookup (map toLower name) colorNameMapping of
  Just code -> Right $ "38;5;" ++ code
  Nothing -> Left $ InvalidColorName name

bgFromName name = case Map.lookup (map toLower name) colorNameMapping of
  Just code -> Right $ "48;5;" ++ code
  Nothing -> Left $ InvalidColorName name

hslToRgb :: Float -> Float -> Float -> (Float, Float, Float)
hslToRgb h s l
  | s == 0    = (l, l, l)
  | otherwise = (hue2rgb p' q' (h + 1/3),
                 hue2rgb p' q' h,
                 hue2rgb p' q' (h - 1/3))
  where
    q' = if l < 0.5 then l * (1 + s) else l + s - l * s
    p' = 2 * l - q'
    hue2rgb p q t
      | t < 0     = hue2rgb p q (t + 1)
      | t > 1     = hue2rgb p q (t - 1)
      | t < 1/6   = p + (q - p) * 6 * t
      | t < 1/2   = q
      | t < 2/3   = p + (q - p) * (2/3 - t) * 6
      | otherwise = p

parseIntRGB :: String -> Either LexerError Int
parseIntRGB s = case reads s of
  [(i, "")] | i >= 0 && i < 256 -> Right i
  _ -> Left $ InvalidRgbColor s

parseStyle :: Parser Style
parseStyle = choice $ map (\s -> try $ string (show s) >> return s) [Bold .. Strikethrough]

parseStyleString :: Parser String
parseStyleString = styleToANSI <$> parseStyle

parseHexColor :: Parser (Either LexerError String)
parseHexColor = do
  _ <- char '#'
  hex <- count 3 hexDigit <|> count 6 hexDigit
  return . Right $ "2;" ++ colorFromHex hex
  where
    colorFromHex [r1,g1,b1] = printf "%d;%d;%d" (hex2dec r1 r1) (hex2dec g1 g1) (hex2dec b1 b1)
    colorFromHex [r1,r2,g1,g2,b1,b2] = printf "%d;%d;%d" (hex2dec r1 r2) (hex2dec g1 g2) (hex2dec b1 b2)
    colorFromHex _ = error "Invalid hex color"
    hex2dec a b = read ['0','x',a,b] :: Int

parseRGBColor :: Parser (Either LexerError String)
parseRGBColor = do
  _ <-string "rgb" >> spaces >> char '('
  rgb <- sepBy (spaces >> many1 digit <* spaces) (oneOf ",; ")
  _ <- char ')'
  return $ case mapM parseIntRGB rgb of
    Right [r, g, b] -> Right $ printf "2;%d;%d;%d" r g b
    _ -> Left $ InvalidRgbColor "Invalid RGB values"

parseFloat :: String -> Parser (Either LexerError Float)
parseFloat s = case readMaybe s of
  Just f  -> return $ Right f
  Nothing -> return $ Left $ InvalidHslColor s

parsePercentage :: String -> Parser (Either LexerError Float)
parsePercentage s = case readMaybe s of
  Just p  -> return $ Right (p / 100)
  Nothing -> return $ Left $ InvalidPercentage s

parseHSLColor :: Parser (Either LexerError String)
parseHSLColor = do
  _ <- string "hsl" >> spaces >> char '('
  h <- parseFloat =<< (spaces >> option "" (string "-") >> many1 digit)
  s <- parsePercentage =<< (spaces >> many1 digit <* optional (char '%'))
  l <- parsePercentage =<< (spaces >> many1 digit <* optional (char '%'))
  _ <- char ')'
  return $ liftM2 (\h' (s', l') -> let (r, g, b) = hslToRgb h' s' l'
                                   in printf "2;%d;%d;%d" (round (r * 255) :: Int) (round (g * 255) :: Int) (round (b * 255) :: Int))
                  h (liftM2 (,) s l)

parseColorName :: Parser (Either LexerError String)
parseColorName = fgFromName <$> many1 (alphaNum <|> char '-')

parseQualifiedColor :: Parser (Either LexerError String)
parseQualifiedColor = do
  qualifier <- optionMaybe (try (string "fg:") <|> try (string "bg:"))
  color <- parseHexColor <|> parseRGBColor <|> parseHSLColor <|> parseColorName
  return $ fmap (qualifyColor qualifier) color

parseTag :: Parser (Either LexerError String)
parseTag = spaces >> (Right . styleToANSI <$> parseStyleInner <|> parseQualifiedColor)
  where parseStyleInner = choice $ map (\s -> string (show s) $> s) [Bold .. Strikethrough]

parseTags :: Parser [Either LexerError String]
parseTags = parseTag `sepBy` many1 (oneOf ", \t")

tagToCode :: String -> Either LexerError [String]
tagToCode = either (Left . InvalidTag . show) sequence . parse parseTags ""
