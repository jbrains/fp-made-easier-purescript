module Test.Main where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, describe, it, pending')
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec (describe)
import Test.Spec (describe)
import Data.Int (rem)

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

snoc :: forall a. List a -> a -> List a
snoc Nil element = singleton element
snoc (x : xs) element = x : snoc xs element

checkSnoc :: Spec Unit
checkSnoc =
    describe "snoc" do
        it "appends to an empty list" do
            (snoc Nil 12) `shouldEqual` (singleton 12)
        it "appends to a non-empty list" do
            (snoc (singleton 7) 12) `shouldEqual` (7 : 12 : Nil)
        it "appends to a longer non-empty list" do
            (snoc (1 : 2 : 3 : 4 : 5 : Nil) 12) `shouldEqual` (1 : 2 : 3 : 4 : 5 : 12 : Nil)

length :: forall a. List a -> Int
length = tailRecursiveLength 0
    where
    tailRecursiveLength :: Int -> List a -> Int
    tailRecursiveLength howManySoFar remainingList = case remainingList of
        Nil -> howManySoFar
        (_ : xs) -> tailRecursiveLength (howManySoFar + 1) xs

checkLength :: Spec Unit
checkLength =
    describe "length" do
        it "empty list" do
            length Nil `shouldEqual` 0
        it "non-empty list" do
            length (1 : 2 : 3 : 4 : 5 : Nil) `shouldEqual` 5

head :: forall a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

checkHead :: Spec Unit
checkHead =
    describe "head" do
        it "empty list" do
            (head (Nil :: List Unit)) `shouldEqual` Nothing
        it "1-item list" do
            (head $ 12 : Nil) `shouldEqual` Just 12
        it "longer list" do
            (head $ 1 : 2 : 3 : 4 : 5 : Nil) `shouldEqual` Just 1

last :: forall a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

checkLast :: Spec Unit
checkLast =
    describe "last" do
        it "empty list" do
            last (Nil :: List Unit) `shouldEqual` Nothing
        it "1-item list" do
            last (1 : Nil) `shouldEqual` Just 1
        it "longer list" do
            last (1 : 2 : 3 : 4 : 5 : Nil) `shouldEqual` Just 5

init :: forall a. List a -> Maybe (List a)
init Nil = Nothing
init xs = Just $ initNonEmpty xs where
    initNonEmpty :: List a -> List a
    initNonEmpty Nil = Nil
    initNonEmpty (_ : Nil) = Nil
    initNonEmpty (y : ys) = y : initNonEmpty ys

checkInit :: Spec Unit
checkInit =
    describe "init" do
        it "empty list" do
            init (Nil :: List Unit) `shouldEqual` Nothing
        it "1-item list" do
            init (1 : Nil) `shouldEqual` Just Nil
        it "longer list" do
            init (1 : 2 : 3 : 4 : 5 : Nil) `shouldEqual` Just (1 : 2 : 3 : 4 : Nil)

uncons :: forall a. List a -> Maybe { head :: a, tail :: List a}
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

checkUncons :: Spec Unit
checkUncons =
    describe "uncons" do
        it "empty list" do
            (uncons (Nil :: List Unit)) `shouldEqual` Nothing
        it "1-item list" do
            uncons (1 : Nil) `shouldEqual` (Just { head: 1, tail: Nil })
        it "longer list" do
            uncons (1 : 2 : 3 : 4 : Nil) `shouldEqual` Just { head: 1, tail: (2 : 3 : 4 : Nil) }

index :: forall a. List a -> Int -> Maybe a
index Nil      _ = Nothing
index _        n | n < 0 = Nothing
index (x : _)  0 = Just x
index (_ : xs) n = index xs (n-1)

infixl 8 index as !!

checkIndex :: Spec Unit
checkIndex =
    describe "index" do
        it "empty list" do
            index (Nil :: List Unit) 0 `shouldEqual` Nothing
            index (Nil :: List Unit) 1 `shouldEqual` Nothing
            index (Nil :: List Unit) 42397 `shouldEqual` Nothing
        describe "non-empty list" do
            it "index within bounds" do
                index (1 : Nil) 0 `shouldEqual` Just 1
                index (1 : 2 : 3 : 4 : 5 : Nil) 2 `shouldEqual` Just 3
            it "index out of bounds" do
                index (1 : Nil) 1 `shouldEqual` Nothing
                index (1 : Nil) 2 `shouldEqual` Nothing
                index (1 : Nil) 273123 `shouldEqual` Nothing
                index (1 : 2 : 3 : 4 : 5 : Nil) (-2) `shouldEqual` Nothing
                index (1 : 2 : 3 : 4 : 5 : Nil) 5 `shouldEqual` Nothing
        describe "use operator" do
            it "works" do
                ((1 : Nil) !! 0) `shouldEqual` (Just 1)


findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex = findIndexStartingFrom 0
    where
    findIndexStartingFrom offset predicate remainingList = case remainingList of
        Nil -> Nothing
        (y : _) | predicate y -> Just offset
        (_ : ys) -> findIndexStartingFrom (offset + 1) predicate ys

checkFindIndex :: Spec Unit
checkFindIndex =
    describe "findIndex" do
        describe "exactly 1 item matches" do
            it "first item matches" do
                findIndex (_ == 10) (10 : Nil) `shouldEqual` Just 0
            it "second item matches" do
                findIndex (_ == 10) (1 : 10 : Nil) `shouldEqual` Just 1
            it "third item matches" do
                findIndex (_ == 10) (1 : 2 : 10 : Nil) `shouldEqual` Just 2
            it "item in the middle matches" do
                findIndex (_ == 10) (1 : 2 : 3 : 10 : 5 : 6 : 7 : Nil) `shouldEqual` Just 3
        describe "several items match" do
            it "starting with the first item" do
                findIndex (_ == 10) (10 : 2 : 10 : 4 : 5 : 10 : Nil) `shouldEqual` Just 0
            it "starting with the second item" do
                findIndex (_ == 10) (1 : 10 : 10 : 4 : 5 : 10 : Nil) `shouldEqual` Just 1
            it "starting somewhere in the middle" do
                findIndex (_ == 10) (1 : 2 : 3 : 10 : 5 : 10 : 7 : Nil) `shouldEqual` Just 3
        describe "no items match" do
            it "empty list" do
                findIndex (const false) Nil `shouldEqual` Nothing
                findIndex (const true) Nil `shouldEqual` Nothing
            it "1-item list" do
                findIndex (const false) (1 : Nil) `shouldEqual` Nothing
            it "several-item list" do
                findIndex (const false) (1 : 2 : 3 : 4 : 5 : Nil) `shouldEqual` Nothing

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex = findLastIndexFromOffset 0 Nothing
    where
        findLastIndexFromOffset offset indexOfLastMatch predicate remainingList = case remainingList of
            Nil -> indexOfLastMatch
            (item : theRest) -> findLastIndexFromOffset (offset + 1) (if predicate item then Just offset else indexOfLastMatch) predicate theRest

checkFindLastIndex :: Spec Unit
checkFindLastIndex =
    describe "findLastIndex" do
        describe "exactly 1 item matches" do
            it "first item matches" do
                findLastIndex (_ == 10) (10 : Nil) `shouldEqual` Just 0
            it "second item matches" do
                findLastIndex (_ == 10) (1 : 10 : Nil) `shouldEqual` Just 1
            it "third item matches" do
                findLastIndex (_ == 10) (1 : 2 : 10 : Nil) `shouldEqual` Just 2
            it "item in the middle matches" do
                findLastIndex (_ == 10) (1 : 2 : 3 : 10 : 5 : 6 : 7 : Nil) `shouldEqual` Just 3
        describe "several items match" do
            it "starting with the last item" do
                findLastIndex (_ == 10) (10 : 2 : 10 : 4 : 5 : 10 : Nil) `shouldEqual` Just 5
            it "starting with the second-last item" do
                findLastIndex (_ == 10) (1 : 10 : 10 : 4 : 10 : 5 : Nil) `shouldEqual` Just 4
            it "starting somewhere in the middle" do
                findLastIndex (_ == 10) (1 : 2 : 3 : 10 : 5 : 10 : 7 : 8 : 9 : Nil) `shouldEqual` Just 5
        describe "no items match" do
            it "empty list" do
                findLastIndex (const false) Nil `shouldEqual` Nothing
                findLastIndex (const true) Nil `shouldEqual` Nothing
            it "1-item list" do
                findLastIndex (const false) (1 : Nil) `shouldEqual` Nothing
            it "several-item list" do
                findLastIndex (const false) (1 : 2 : 3 : 4 : 5 : Nil) `shouldEqual` Nothing

reverse :: List ~> List
reverse = prependAllFrom Nil
    where
        prependAllFrom to from = case from of
            Nil -> to
            (y : ys) -> prependAllFrom (y : to) ys

checkReverse :: Spec Unit
checkReverse =
    describe "reverse" do
        it "empty list" do
            reverse (Nil :: List Unit) `shouldEqual` (Nil :: List Unit)
        it "1-item list" do
            reverse (5 : Nil) `shouldEqual` (5 : Nil)
        it "2-item list" do
            reverse (5 : 3 : Nil) `shouldEqual` (3 : 5 : Nil)
        it "3-item list" do
            reverse (7 : 5 : 3 : Nil) `shouldEqual` (3 : 5 : 7 : Nil)

concat :: forall a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

checkConcat :: Spec Unit
checkConcat =
    describe "concat" do
        it "empty list" do
            concat (Nil :: List (List Unit)) `shouldEqual` (Nil :: List Unit)
        it "1-list list" do
            concat ((1 : Nil) : Nil) `shouldEqual` (1 : Nil)
        it "1-list list with 3 items" do
            concat ((1 : 2 : 3 : Nil) : Nil) `shouldEqual` (1 : 2 : 3 : Nil)
        it "several-list list" do
            concat ((1 : Nil) : (2 : Nil) : (3 : Nil) : Nil) `shouldEqual` (1 : 2 : 3 : Nil)
        it "several-list list of several items each" do
            concat ((1 : 2 : 3 : Nil) : (4 : Nil) : (5 : 6 : Nil) : Nil : (7 : 8 : 9 : Nil) : Nil) `shouldEqual` (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : Nil)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter predicate = reverse <<< filterWhileCollectingMatchesInReverseSequence Nil
    where
    filterWhileCollectingMatchesInReverseSequence matched remaining = case remaining of
        Nil -> matched
        (y : ys) -> if predicate y then filterWhileCollectingMatchesInReverseSequence (y : matched) ys else filterWhileCollectingMatchesInReverseSequence matched ys

checkFilter :: Spec Unit
checkFilter =
    describe "filter" do
        it "empty list" do
            filter (const true) (Nil :: List Unit) `shouldEqual` (Nil :: List Unit)
        describe "1-item list" do
            it "item matches" do
                filter (const true) (1 : Nil) `shouldEqual` (1 : Nil)
            it "item doesn't match" do
                filter (const false) (1 : Nil) `shouldEqual` (Nil)
        describe "several-item list" do
            it "all items match" do
                filter (const true) (1 : 2 : 3 : Nil) `shouldEqual` (1 : 2 : 3 : Nil)
            it "no items match" do
                filter (const false) (1 : 2 : 3 : Nil) `shouldEqual` (Nil)
            it "more than 1 item matches" do
                filter (\n -> rem n 2 == 0) (1 : 2 : 3 : 4 : 5 : 6 : Nil) `shouldEqual` (2 : 4 : 6 : Nil)

catMaybes :: forall a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (Just x : xs) = x : catMaybes xs
catMaybes (Nothing : xs) = catMaybes xs

checkCatMaybes :: Spec Unit
checkCatMaybes =
    describe "catMaybes" do
        it "empty list" do
            catMaybes (Nil :: List (Maybe Unit)) `shouldEqual` (Nil :: List Unit)
        describe "1-item list" do
            it "Nothing" do
                catMaybes ((Nothing :: Maybe Unit) : Nil) `shouldEqual` (Nil :: List Unit)
            it "Just" do
                catMaybes (Just unit : Nil) `shouldEqual` (unit : Nil)
        describe "several-item list" do
            it "Nothings" do
                catMaybes ((Nothing :: Maybe Unit) : (Nothing :: Maybe Unit) : (Nothing :: Maybe Unit) : (Nothing :: Maybe Unit) : Nil) `shouldEqual` (Nil :: List Unit)
            it "some Justs" do
                catMaybes (Just unit : Nothing : Nothing : Just unit : Nothing : Just unit : Just unit : Nothing : Nothing : Nil) `shouldEqual` (unit : unit : unit : unit : Nil)

range :: Int -> Int -> List Int
range from to = rangeByCollectingInReverseSequence Nil (if from > to then (-1) else 1) from to
    where
    rangeByCollectingInReverseSequence collected step from to
        | from == to = to : collected
        | otherwise = rangeByCollectingInReverseSequence (to : collected) step from (to - step)

checkRange :: Spec Unit
checkRange =
    describe "range" do
        it "singleton" do
            range 5 5 `shouldEqual` (5 : Nil)
        it "increasing" do
            range 5 6 `shouldEqual` (5 : 6 : Nil)
            range 4 7 `shouldEqual` (4 : 5 : 6 : 7 : Nil)
        it "decreasing" do
            range 5 4 `shouldEqual` (5 : 4 : Nil)
            range 6 3 `shouldEqual` (6 : 5 : 4 : 3 : Nil)

checkDataList :: Spec Unit
checkDataList =
    describe "Data.List" do
        checkSingleton
        checkNull
        checkSnoc
        checkLength
        checkHead
        checkLast
        checkInit
        checkUncons
        checkIndex
        checkFindIndex
        checkFindLastIndex
        checkReverse
        checkConcat
        checkFilter
        checkCatMaybes
        checkRange

main :: Effect Unit
main = do
    launchAff_ $ runSpec [ consoleReporter] do
        checkPrelude
        checkDataList