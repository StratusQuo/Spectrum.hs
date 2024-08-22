module Spectrum.Colors
  ( -- Basic colors
    black, red, green, yellow, blue, magenta, cyan, white
  , -- Bright colors
    brightBlack, brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan, brightWhite, lightSteelBlue
  , -- Styles
    bold, dim, italic, underline, blink, rapidBlink, inverse, hidden, doubleUnderline, strikethrough
  , -- Color functions
    rgb, hex, colorName
  , -- Background colors
    bgBlack, bgRed, bgGreen, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite
  , -- Function to get any color by name
    fromName
  ) where

import qualified Data.Map as Map
import Spectrum.Lexer
import Text.Parsec

-- ╭─────────────────────────────────────────────╮
-- │ Function to apply ANSI codes                │  
-- ╰─────────────────────────────────────────────╯
applyANSI :: String -> String -> String
applyANSI code text = "\ESC[" ++ code ++ "m" ++ text ++ "\ESC[0m"

-- helper function to convert Either LexerError String to String -> Either LexerError String
liftColorFunc :: Either LexerError String -> (String -> Either LexerError String)
liftColorFunc (Right code) = const (Right code)
liftColorFunc (Left err) = const (Left err)

-- Function to apply any color or style
applyColor :: (String -> Either LexerError String) -> String -> String
applyColor colorFunc text = case colorFunc text of
  Right code -> applyANSI code text
  Left _ -> text  -- fallback to original text if there's an error

-- Basic colors
black, red, green, yellow, blue, magenta, cyan, white :: String -> String
black = applyColor $ liftColorFunc $ fgFromName "black"
red = applyColor $ liftColorFunc $ fgFromName "red"
green = applyColor $ liftColorFunc $ fgFromName "green"
yellow = applyColor $ liftColorFunc $ fgFromName "yellow"
blue = applyColor $ liftColorFunc $ fgFromName "blue"
magenta = applyColor $ liftColorFunc $ fgFromName "magenta"
cyan = applyColor $ liftColorFunc $ fgFromName "cyan"
white = applyColor $ liftColorFunc $ fgFromName "white"

-- Bright colors
brightBlack, brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan, brightWhite :: String -> String
brightBlack = applyColor $ liftColorFunc $ fgFromName "bright-black"
brightRed = applyColor $ liftColorFunc $ fgFromName "bright-red"
brightGreen = applyColor $ liftColorFunc $ fgFromName "bright-green"
brightYellow = applyColor $ liftColorFunc $ fgFromName "bright-yellow"
brightBlue = applyColor $ liftColorFunc $ fgFromName "bright-blue"
brightMagenta = applyColor $ liftColorFunc $ fgFromName "bright-magenta"
brightCyan = applyColor $ liftColorFunc $ fgFromName "bright-cyan"
brightWhite = applyColor $ liftColorFunc $ fgFromName "bright-white"

-- Background colors
bgBlack, bgRed, bgGreen, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite :: String -> String
bgBlack = applyColor $ liftColorFunc $ bgFromName "black"
bgRed = applyColor $ liftColorFunc $ bgFromName "red"
bgGreen = applyColor $ liftColorFunc $ bgFromName "green"
bgYellow = applyColor $ liftColorFunc $ bgFromName "yellow"
bgBlue = applyColor $ liftColorFunc $ bgFromName "blue"
bgMagenta = applyColor $ liftColorFunc $ bgFromName "magenta"
bgCyan = applyColor $ liftColorFunc $ bgFromName "cyan"
bgWhite = applyColor $ liftColorFunc $ bgFromName "white"

--


-- Styles
bold, dim, italic, underline, blink, rapidBlink, inverse, hidden, strikethrough, doubleUnderline :: String -> String
bold = applyANSI "1"
dim = applyANSI "2"
italic = applyANSI "3"
underline = applyANSI "4"
blink = applyANSI "5"
rapidBlink = applyANSI "6"
inverse = applyANSI "7"
hidden = applyANSI "8"
strikethrough = applyANSI "9"
-- ! Experimental!
doubleUnderline = applyANSI "21"

-- ╭─────────────────────────────────────────────╮
-- │ Function to map RGB Values to ANSI Codes    │  
-- ╰─────────────────────────────────────────────╯
rgb :: Int -> Int -> Int -> String -> String
rgb r g b = applyANSI $ "38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b

-- ╭─────────────────────────────────────────────╮
-- │  Map Hexadecimal Values to ANSI Codes       │  
-- ╰─────────────────────────────────────────────╯
hex :: String -> String -> String
hex hexCode = applyColor $ \_ -> case parse parseHexColor "" hexCode of
    Left err -> Left $ InvalidHexColor (show err)
    Right (Right code) -> Right $ "38;" ++ code
    --Right (Right code) -> Right $ "38;2" ++ code
    Right (Left err) -> Left err

colorName :: String -> String -> String
colorName name = applyColor $ liftColorFunc $ fgFromName name

fromName :: String -> (String -> String)
fromName name = applyColor $ liftColorFunc $ fgFromName name

-- ! Testing Shorter Names
lightSteelBlue :: String -> String
lightSteelBlue = applyColor $ liftColorFunc $ fgFromName "light-steel-blue"

-- Generate functions for all named colors
-- This can be used to create additional color functions as needed
_allNamedColors :: Map.Map String (String -> String)
_allNamedColors = Map.map fromName colorNameMapping