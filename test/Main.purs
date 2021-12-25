module Test.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, show, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

flip :: forall a b c. (a -> b -> c) -> (b -> a -> c)
flip f = \x y -> f y x

checkFlip :: Spec Unit
checkFlip =
    describe "flip" do
        it "applies the correct argument" do
            (flip const 1 2) `shouldEqual` 2

const :: forall a b. a -> (b -> a)
const x = \_ -> x

checkConst :: Spec Unit
checkConst =
    describe "const" do
        it "returns its argument" do
            const "::return me::" ignoreMe `shouldEqual` "::return me::"
            where
            ignoreMe = 12

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

checkApplyOperator :: Spec Unit
checkApplyOperator =
    describe "$" do
        it "applies the argument correctly" do
            (show $ flip const 1 2) `shouldEqual` "2"

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped x f = f x

checkApplyFlippedOperator :: Spec Unit
checkApplyFlippedOperator =
    describe "#" do
        it "applies the argument correctly" do
            (flip const 1 2) `applyFlipped` show `shouldEqual` "2"

main :: Effect Unit
main = do
    launchAff_ $ runSpec [ consoleReporter] do
        checkFlip
        checkConst
        checkApplyOperator
        checkApplyFlippedOperator

