{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Spectrum.Spectrum 
  ( module Spectrum.Capabilities
  , module Spectrum.Lexer
  , module Spectrum.Colors -- ! Test
  , Printer(..)
  , makePrinter
  , Exn
  , Noexn
  , simpleSprintf
  ) where

import Spectrum.Capabilities
import Spectrum.Colors -- ! Test
import Spectrum.Lexer
import Data.List (intercalate)
import System.IO
import Control.Exception ()
import Text.Printf (printf, PrintfType)
import Data.IORef (newIORef, writeIORef, modifyIORef, readIORef)
import Data.Char (toLower)
import System.IO.Unsafe (unsafePerformIO)

stackToEsc :: [String] -> String
stackToEsc stack = "\ESC[" ++ intercalate ";" (reverse stack) ++ "m"

data Printer = Printer
  { preparePPF :: Handle -> IO (IO ())
  , simpleModule :: SimpleModule
  }

data SimpleModule = SimpleModule
  { simplePrintf :: forall a. PrintfType a => String -> a
  , simpleEprintf :: forall a. PrintfType a => String -> a
  , simpleSprintf :: forall a. PrintfType a => String -> a
  }

makePrinter :: Bool -> Printer
makePrinter raiseErrors = Printer {..}
  where
    preparePPF handle = do
      let originalState = ["0"]  -- Start with reset code
      ref <- newIORef originalState
      let reset = do
            writeIORef ref originalState
            hPutStr handle (stackToEsc originalState)
            hFlush handle
      
      let updateStack f = modifyIORef ref f >> readIORef ref >>= hPutStr handle . stackToEsc
      
      let _markOpenTag tag = updateStack $ \stack ->
            case tagToCode (map toLower tag) of
              Right codes -> codes ++ stack
              Left err    -> if raiseErrors then error (show err) else stack
      
      let _markCloseTag _ = updateStack tail
      
      hSetEncoding handle utf8
      return reset

    simpleModule = SimpleModule {..}
      where
        simplePrintf :: forall a. PrintfType a => String -> a
        simplePrintf format = unsafePerformIO $ do
          reset <- preparePPF stdout
          let result = printf format
          putStr result
          reset
          return (printf format)  -- Return the result of printf directly

        simpleEprintf :: forall a. PrintfType a => String -> a
        simpleEprintf format = unsafePerformIO $ do
          reset <- preparePPF stderr
          let result = printf format
          hPutStr stderr result
          reset
          return (printf format)  -- Return the result of printf directly
        simpleSprintf :: forall a. PrintfType a => String -> a
        simpleSprintf = printf

-- ! Need to refine these at some point:
type Exn = Printer
type Noexn = Printer