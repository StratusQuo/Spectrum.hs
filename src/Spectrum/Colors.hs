module Spectrum.Colors
  ( -- Basic colors
    black, red, green, yellow, blue, magenta, cyan, white
  , -- Bright colors
    brightBlack, brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan, brightWhite
  , -- Styles
    bold, dim, italic, underline, blink, rapidBlink, inverse, hidden, strikethrough
  , -- Color functions
    rgb, hex, colorName
  , -- Background colors
    bgBlack, bgRed, bgGreen, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite
  , -- Function to get any color by name
    fromName
  ) where

import qualified Data.Map as Map
import Spectrum.Lexer

-- General function to apply ANSI codes
applyANSI :: String -> String -> String
applyANSI code text = "\ESC[" ++ code ++ "m" ++ text ++ "\ESC[0m"

-- Function to apply any color or style
applyColor :: (String -> Either LexerError String) -> String -> String
applyColor colorFunc text = case colorFunc text of
  Right code -> applyANSI code text
  Left _ -> text  -- fallback to original text if there's an error

-- Basic colors
black, red, green, yellow, blue, magenta, cyan, white :: String -> String
black = applyColor (fgFromName "black")
red = applyColor (fgFromName "red")
green = applyColor (fgFromName "green")
yellow = applyColor (fgFromName "yellow")
blue = applyColor (fgFromName "blue")
magenta = applyColor (fgFromName "magenta")
cyan = applyColor (fgFromName "cyan")
white = applyColor (fgFromName "white")

-- Bright colors
brightBlack, brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan, brightWhite :: String -> String
brightBlack = applyColor (fgFromName "bright-black")
brightRed = applyColor (fgFromName "bright-red")
brightGreen = applyColor (fgFromName "bright-green")
brightYellow = applyColor (fgFromName "bright-yellow")
brightBlue = applyColor (fgFromName "bright-blue")
brightMagenta = applyColor (fgFromName "bright-magenta")
brightCyan = applyColor (fgFromName "bright-cyan")
brightWhite = applyColor (fgFromName "bright-white")

-- Styles
bold, dim, italic, underline, blink, rapidBlink, inverse, hidden, strikethrough :: String -> String
bold = applyANSI "1"
dim = applyANSI "2"
italic = applyANSI "3"
underline = applyANSI "4"
blink = applyANSI "5"
rapidBlink = applyANSI "6"
inverse = applyANSI "7"
hidden = applyANSI "8"
strikethrough = applyANSI "9"

-- Color functions
rgb :: Int -> Int -> Int -> String -> String
rgb r g b = applyANSI $ "38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b

hex :: String -> String -> String
hex hexCode = applyColor (\_ -> parseHexColor hexCode >>= \case Right code -> Right $ "38;" ++ code; Left e -> Left e)

colorName :: String -> String -> String
colorName name = applyColor (fgFromName name)

-- Background colors
bgBlack, bgRed, bgGreen, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite :: String -> String
bgBlack = applyColor (bgFromName "black")
bgRed = applyColor (bgFromName "red")
bgGreen = applyColor (bgFromName "green")
bgYellow = applyColor (bgFromName "yellow")
bgBlue = applyColor (bgFromName "blue")
bgMagenta = applyColor (bgFromName "magenta")
bgCyan = applyColor (bgFromName "cyan")
bgWhite = applyColor (bgFromName "white")

-- Function to get any color by name
fromName :: String -> (String -> String)
fromName name = applyColor (fgFromName name)

-- Generate functions for all named colors
-- This can be used to create additional color functions as needed
allNamedColors :: Map.Map String (String -> String)
allNamedColors = Map.map (\_ name -> fromName name) colorNameMapping