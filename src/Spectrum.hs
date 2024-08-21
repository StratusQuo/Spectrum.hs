module Spectrum.Spectrum where

import Spectrum.Capabilities
import Spectrum.Lexer
import Data.List
import System.IO
import Control.Monad.State

type ANSI = String

data Style = Bold
           | Dim
           | Italic
           | Underline
           | Blink
           | RapidBlink
           | Inverse
           | Hidden
           | Strikethrough
  deriving (Show, Eq)

styleToANSI :: Style -> ANSI
styleToANSI style =
  case style of
    Bold -> "1"
    Dim -> "2"
    Italic -> "3"
    Underline -> "4"
    Blink -> "5"
    RapidBlink -> "5"
    Inverse -> "7"
    Hidden -> "8"
    Strikethrough -> "9"

colorToANSI :: String -> ANSI
colorToANSI color =
  case tagToCode color of
    Right code -> "\ESC[" ++ code ++ "m"
    Left _ -> "" -- Handle errors appropriately (e.g., logging or throwing an exception)

stackToANSI :: [ANSI] -> ANSI
stackToANSI stack = "\ESC[" ++ intercalate ";" (reverse stack) ++ "m"

type PPFState = [ANSI]

preparePPF :: Handle -> IO (IO ())
preparePPF handle = do
  -- Get initial state (empty stack)
  let initialState = []
  -- Run the stateful computation
  let formattedOutput = (`execStateT` initialState) $ do
        -- Print the initial reset code
        liftIO $ hPutStr handle $ stackToANSI ["0"]
        -- Return a reset function
        return $ do
          liftIO $ hPutStr handle $ stackToANSI ["0"]
          liftIO $ hFlush handle
  -- Return the reset function
  return (snd formattedOutput)

simplePrintf :: String -> IO ()
simplePrintf str = do
  reset <- preparePPF stdout
  putStr str
  reset

simpleSprintf :: String -> String -> String
simpleSprintf formatStr str =
  let reset = preparePPF stdout
      result = formatStr ++ str
  in result ++ stackToANSI ["0"] -- Add reset code at the end