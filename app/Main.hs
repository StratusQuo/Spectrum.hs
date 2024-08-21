module Main where

import Spectrum.Spectrum
import System.Environment (lookupEnv)
import System.IO
import Control.Monad (forM_)

main :: IO ()
main = do
  putStrLn "========================="
  putStrLn " Spectrum-HS Test Suite "
  putStrLn "========================="

  -- Basic color tests
  putStrLn $ yellow "Yellow text"
  putStrLn $ red "Red text"
  putStrLn $ green "Green text"

  -- Extended Tests

  putStrLn $ red "This is red text"
  putStrLn $ blue $ bold "This is bold blue text"
  putStrLn $ fromName "deep-sky-blue-4a" "This is deep sky blue text"
  putStrLn $ bgYellow $ black "Black text on yellow background"
  putStrLn $ white $ bgRed $ bold "Bold white text on a Red background"
  putStrLn $ rgb 100 150 200 "Custom RGB color (Pale Blue)"
  putStrLn $ hex "#FF9680" "Custom Hex color (Orange)"

  -- Compound style tests
  putStrLn $ green (bold (underline "Bold underlined green text"))
  
  -- RGB color test
  putStrLn $ rgb 100 150 200 "Custom RGB color"

  -- Hex color test
  putStrLn $ hex "#FF5733" "Custom Hex color"

  -- Background color test
  putStrLn $ bgRed "Red background"

  -- Complex formatting test
  let complexText = lightSteelBlue (bold (underline "Hello " ++ "there" ++ " you" ++ " again " ++ strikethrough "mate"))
  putStrLn $ "pre " ++ complexText ++ " @{<bg:red,#FFd833>warning@} post\n"

  -- Test error handling (if implemented)
  putStrLn "Testing invalid color (should show an error or fallback):"
  putStrLn $ colorName "invalid-color" "This should handle the error gracefully"

  -- Test color level detection
  colorLevel <- supportedColorLevel defaultConfig True
  putStrLn $ "Detected color support level: " ++ show colorLevel

  -- Test environment variable effects
  putStrLn "\nTesting environment variable effects:"
  forM_ ["FORCE_COLOR", "NO_COLOR", "TERM", "COLORTERM"] $ \envVar -> do
    value <- lookupEnv envVar
    putStrLn $ envVar ++ ": " ++ show value

  putStrLn "\nTest complete!"


-- -- Helper functions (implement these in your Spectrum module if not already present)
-- yellow, red, green, bold, underline, strikethrough, bgRed :: String -> String
-- yellow = id  -- Placeholder implementations
-- red = id
-- green = id
-- bold = id
-- underline = id
-- strikethrough = id
-- bgRed = id

-- rgb :: Int -> Int -> Int -> String -> String
-- rgb _ _ _ = id

-- hex :: String -> String -> String
-- hex _ = id

-- lightSteelBlue :: String -> String
-- lightSteelBlue = id

-- colorName :: String -> String -> String
-- colorName _ = id