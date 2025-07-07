{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardinality
import GHC.Generics
import Test.Hspec

data Color = Red | Green | Blue
  deriving (Generic, Show, Eq, TypeCardinality)

main :: IO ()
main = hspec $ do
  describe "cardinality of custom data types" $ do
    it "Color has cardinality 3" $ do
      cardinality @Color `shouldBe` Left 3
    it "[Color] has cardinality infinity" $ do
      cardinality @[Color] `shouldBe` Right "infinity"
