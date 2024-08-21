module Spec where

import Test.Hspec
import Spectrum.Spectrum
import Spectrum.Capabilities
import Spectrum.Lexer

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Spectrum.Lexer" $ do
    it "parses valid color names" $ do
      tagToCode "red" `shouldBe` Right ["38;5;9"]
      tagToCode "bg:blue" `shouldBe` Right ["48;5;12"]

    it "handles invalid color names" $ do
      tagToCode "invalid-color" `shouldBe` Left (InvalidColorName "invalid-color")

  describe "Spectrum.Capabilities" $ do
    it "detects color support levels" $ do
      level <- supportedColorLevel defaultConfig True
      level `shouldSatisfy` (`elem` [Unsupported, Basic, EightBit, TrueColor])

  describe "Spectrum.Spectrum" $ do
    it "generates ANSI escape codes" $ do
      stackToEsc ["1", "31"] `shouldBe` "\ESC[1;31m"

    it "handles compound styles" $ do
      -- Implement a test for compound styles
      pending
