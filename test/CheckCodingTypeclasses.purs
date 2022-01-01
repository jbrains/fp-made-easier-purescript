module Test.CheckCodingTypeclasses where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data Maybe a = Nothing | Just a

instance showMaybe :: Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "Just " <> show x

instance eqMaybe :: Eq a => Eq (Maybe a) where
    eq Nothing Nothing = true
    eq (Just x) (Just y) = x == y
    eq _ _ = false

checkCodingTypeclasses :: Spec Unit
checkCodingTypeclasses =
    describe "Maybe" do
        it "exists, so it implements Show and Eq" do
            Just 3 `shouldEqual` Just 3