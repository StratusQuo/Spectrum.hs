module Main where

import Spectrum.Spectrum
import System.IO

main :: IO ()
main = do
  putStrLn $ yellow "before"
  simplePrintf $ green "Hello world 👋\n"
  simplePrintf $ green <> bold <> yellow $ "Redundant fg color in compound tag (last specified wins)\n"

  let result = simpleSprintf "pre %s @{<bg:red,#FFd833>warning@} post\n"
                (lightSteelBlue <> bold $ "Hello " <> underline "there" <> " you" <> " again " <> strikethrough "mate")

  putStrLn result

  putStrLn $ red "after"

  putStrLn stdout $ green "using putStrLn 👋"

  -- Note: Format.str_formatter equivalent in Haskell is more involved
  -- and would require a more complex implementation.
  -- For now, we'll skip the last part of the OCaml example.

  putStrLn $ red "after reset ()"