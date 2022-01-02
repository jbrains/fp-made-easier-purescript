module Test.CheckFoldByHand where

import Prelude

import Data.List.Types (List(..), (:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

foldl :: forall a b. (b -> a -> b) -> b -> List a -> b
foldl _ accumulator Nil = accumulator
foldl f accumulator (x : xs) = foldl f (f accumulator x) xs

foldr :: forall a b. (b -> a -> b) -> b -> List a -> b
foldr _ accumulator Nil = accumulator
foldr f accumulator (x : xs) = f (foldr f accumulator xs) x

checkFoldByHand :: Spec Unit
checkFoldByHand  =
    describe "fold, written by hand" do
        describe "foldl" do
            it "empty" do
                (foldl (\_ _ -> "hello") "" (Nil :: List Int)) `shouldEqual` ""
            it "1-item list" do
                (foldl (\_ _ -> "hello") "" (1 : Nil)) `shouldEqual` "hello"
            it "2-item list" do
                (foldl (\_ _ -> "hello") "" (1 : 2 : Nil)) `shouldEqual` "hello"
        describe "foldr" do
            it "empty" do
                (foldr (\_ _ -> "hello") "" (Nil :: List Int)) `shouldEqual` ""
            it "1-item list" do
                (foldr (\_ _ -> "hello") "" (1 : Nil)) `shouldEqual` "hello"
            it "2-item list" do
                (foldr (+) 0 (1 : 2 : Nil)) `shouldEqual` 3
