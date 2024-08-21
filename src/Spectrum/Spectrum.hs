{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Spectrum.Spectrum 
  ( module Spectrum.Capabilities
  , module Spectrum.Lexer
  , Printer(..)
  , makePrinter
  , Exn
  , Noexn
  ) where

import Spectrum.Capabilities
import Spectrum.Lexer
import Data.List (intercalate)
import System.IO
import Control.Monad.State
import Control.Exception (catch, throw)
import Text.Printf (printf)

type ANSI = String

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
      
      let markOpenTag tag = updateStack $ \stack ->
            case tagToCode (map toLower tag) of
              Right code -> code : stack
              Left err   -> if raiseErrors then throw err else stack
      
      let markCloseTag _ = updateStack tail
      
      hSetEncoding handle utf8
      return reset

    simpleModule = SimpleModule {..}
      where
        simplePrintf format = unsafePerformIO $ do
          reset <- preparePPF stdout
          let result = printf format
          reset
          return result

        simpleEprintf format = unsafePerformIO $ do
          reset <- preparePPF stderr
          let result = hPrintf stderr format
          reset
          return result

        simpleSprintf = printf

type Exn = Printer
type Noexn = Printer

exn :: Exn
exn = makePrinter True

noexn :: Noexn
noexn = makePrinter False