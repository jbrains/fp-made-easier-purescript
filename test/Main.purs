module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Data.List (List(..), (:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec (pending')

checkFlip :: Spec Unit
checkFlip =
    describe "flip" do
        it "applies the correct argument" do
            (flip const 1 2) `shouldEqual` 2

checkConst :: Spec Unit
checkConst =
    describe "const" do
        it "returns its argument" do
            const "::return me::" ignoreMe `shouldEqual` "::return me::"
            where
            ignoreMe = 12

checkApplyOperator :: Spec Unit
checkApplyOperator =
    describe "$" do
        it "applies the argument correctly" do
            (show $ flip const 1 2) `shouldEqual` "2"

checkApplyFlippedOperator :: Spec Unit
checkApplyFlippedOperator =
    describe "#" do
        it "applies the argument correctly" do
            (flip const 1 2 # show) `shouldEqual` "2"

checkPrelude :: Spec Unit
checkPrelude =
    describe "Prelude" do
       checkFlip
       checkConst
       checkApplyOperator
       checkApplyFlippedOperator

singleton :: forall a. a -> List a
singleton x = x : Nil

checkSingleton :: Spec Unit
checkSingleton =
    describe "singleton" do
        it "matches creating a List the long way" do
            (singleton 12) `shouldEqual` (12 : Nil)

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

checkNull :: Spec Unit
checkNull =
    describe "null" do
        it "handles an empty list" do
            null Nil `shouldEqual` true
        it "handles an non-empty list" do
            (null $ singleton 12) `shouldEqual` false

checkDataList :: Spec Unit
checkDataList =
    describe "Data.List" do
        checkSingleton
        checkNull

main :: Effect Unit
main = do
    launchAff_ $ runSpec [ consoleReporter] do
        checkPrelude
        checkDataList