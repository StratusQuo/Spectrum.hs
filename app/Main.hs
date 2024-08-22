-- ╭─────────────────────────────────────────────╮
-- │ Main (Mostly for Sanity Testing)            │  
-- ╰─────────────────────────────────────────────╯

module Main where

import Spectrum.Spectrum
import System.Environment (lookupEnv)
import System.IO
import Control.Monad (forM_)

-- | Making "|>" an alias for "$" to test a few things out
-- | and hopefully make things a bit more readable:
infixr 0 |>
(|>) :: (a -> b) -> a -> b
(|>) = ($)


-- | Sanity Tests:
 
main :: IO ()
main = do
  putStrLn "\n"
  putStrLn "=========================="
  putStrLn "  Spectrum-HS Test Suite  "
  putStrLn "=========================="
  putStrLn "\n"

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
  putStrLn $ hex "FF9680" "Custom Hex color (Orange)"
  -- Compound style tests
  putStrLn $ green (bold (underline "Bold underlined green text"))
  -- RGB color test
  putStrLn $ rgb 100 150 200 "Custom RGB color (lighter blue)"
  -- Hex color test
  putStrLn $ hex "#FF5733" "Custom Hex color (Deeper Orange)"
  -- Background color test
  putStrLn $ bgRed "Red background"

  putStrLn "\n"
  putStrLn "========================="
  putStrLn "       Style Test        "
  putStrLn "========================="
  putStrLn "\n"

  putStrLn $ red $ bold "Bold Red Text"
  putStrLn $ red $ dim "Dim Red Text"
  putStrLn $ yellow $ italic "Yellow Italic Text"
  putStrLn $ green $ underline "Green Underline Text"
  putStrLn $ blue $ blink "Blue Blinking Text"
  putStrLn $ rgb 100 150 200 $ rapidBlink "Pale Blue Rapidly Blinking Text"
  putStrLn $ green $ inverse "Inverse Green Text"
  -- What happens if we put a pipe operator here....
  putStrLn |> cyan |> strikethrough "Strikethrough Cyan Text"
  putStrLn $ cyan "Regular Cyan Text"
  putStrLn $ blue $ doubleUnderline "Double Underlined Blue Text"

  putStrLn "\n"
  putStrLn "========================="
  putStrLn "   256-Color Test        "
  putStrLn "========================="
  putStrLn "\n"

  putStrLn $ red $ bold "Red Text" 
  putStrLn $ yellow $ bold "Yellow Text" 
  putStrLn $ green $ bold "Green Text"
  putStrLn $ maroon $ bold "Maroon Text"
  putStrLn $ olive $ bold "Olive Text" 
  putStrLn $ purple $ bold "Purple Text"
  putStrLn $ teal $ bold "Teal Text"
  putStrLn $ silver $ bold "White Text" 
  putStrLn $ grey $ bold "Grey Text"
  putStrLn $ lime $ bold "Lime Text"
  putStrLn $ blue $ bold "Blue Text"
  putStrLn $ fuchsia $ bold "Fucshia Text" 
  putStrLn $ cyan $ bold "Cyan Text"
  putStrLn $ magenta $ bold "Magenta Text"



  putStrLn "\n"
  putStrLn "========================="
  putStrLn " Complex Formatting Test "
  putStrLn "========================="
  putStrLn "\n"
  let complexText = lightSteelBlue (bold (underline "Hello" ++ " there," ++ " you" ++ " again " ++ strikethrough "mate?"))
  putStrLn $ "No Color -- " ++ complexText ++ "\n @{<bg:red,#FFd833>warning@} After\n"

  -- Test error handling (if implemented)
  putStrLn "\n"
  putStrLn "========================="
  putStrLn "   Error Handling Test   "
  putStrLn "========================="
  putStrLn "\n"
  putStrLn "Testing invalid color (should show an error or fallback):\n"
  putStrLn $ colorName "invalid-color" "This should handle the error gracefully"

  -- Test color level detection
  colorLevel <- supportedColorLevel defaultConfig True
  putStrLn $ "Detected color support level: " ++ show colorLevel

  -- Test environment variable effects
  putStrLn "\nTesting environment variable effects:\n"
  forM_ ["FORCE_COLOR", "NO_COLOR", "TERM", "COLORTERM"] $ \envVar -> do
    value <- lookupEnv envVar
    putStrLn $ envVar ++ ": " ++ show value

  putStrLn "\nTest complete!"