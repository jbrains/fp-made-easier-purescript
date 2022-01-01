module Test.CheckCodingTypeclasses where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions (shouldNotEqual)

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

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
    show = genericShow

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)

checkCodingTypeclasses :: Spec Unit
checkCodingTypeclasses =
    describe "Handcoding typeclasses" do
        describe "Either" do
            it "exists, so it implements Show and Eq" do
                (Right 3 :: Either String Int) `shouldEqual` (Right 3)
                (Left "fuck" :: Either String Int) `shouldEqual` (Left "fuck")
                (Right 3) `shouldNotEqual` (Left "fuck")
            it "implements Ord" do
                compare (Left "fuck") (Right 3) `shouldEqual` LT
                compare (Left "fuck" :: Either String Int) (Left "fuckish") `shouldEqual` LT
                compare (Left "fuck" :: Either String Int) (Left "before fuck") `shouldEqual` GT
                compare (Right 3 :: Either String Int) (Right 3) `shouldEqual` EQ
                compare (Right 2 :: Either String Int) (Right 3) `shouldEqual` LT
                compare (Right 4 :: Either String Int) (Right 3) `shouldEqual` GT
            it "greaterThanOrEq" do
                greaterThanOrEq (Left "fuck") (Right 3) `shouldEqual` false
                greaterThanOrEq (Right 3) (Left "fuck") `shouldEqual` true
                greaterThanOrEq (Right 3 :: Either String Int) (Right 2) `shouldEqual` true
                greaterThanOrEq (Right 3 :: Either String Int) (Right 3) `shouldEqual` true
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
