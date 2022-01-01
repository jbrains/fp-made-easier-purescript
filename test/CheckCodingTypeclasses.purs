module Test.CheckCodingTypeclasses where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
    show = genericShow

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance ordMaybe :: Ord a => Ord (Maybe a)

greaterThanOrEq :: forall a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = comparison == GT || comparison == EQ
    where
    comparison = compare x y

checkCodingTypeclasses :: Spec Unit
checkCodingTypeclasses =
    describe "Maybe" do
        it "exists, so it implements Show and Eq" do
            Just 3 `shouldEqual` Just 3
        it "implements Ord" do
            (compare Nothing $ Just 4) `shouldEqual` LT
            (compare Nothing $ (Nothing :: Maybe Int)) `shouldEqual` EQ
            (compare (Just 4) Nothing) `shouldEqual` GT
            (compare (Just 4) (Just 4)) `shouldEqual` EQ
            (compare (Just 4) (Just 3)) `shouldEqual` GT
            (compare (Just 3) (Just 4)) `shouldEqual` LT
        it "greaterThanOrEq" do
            greaterThanOrEq (Just 4) (Just 4) `shouldEqual` true
            greaterThanOrEq (Just 5) (Just 4) `shouldEqual` true
