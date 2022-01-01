module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.CheckDataList (checkDataList)
import Test.CheckPrelude (checkPrelude)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec (Spec)
import Test.Spec (describe)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)

type Address =
    { street1 :: String
    , street2 :: String
    , city :: String
    , state :: String
    , zip :: String
    }

data Person = Person
    { name :: String
    , age :: Int
    , address :: Address
    }

instance eqPerson :: Eq Person where
    eq (Person lhs) (Person rhs) = lhs == rhs

instance showPerson :: Show Person where
    show (Person {name, age, address: {street1, street2, city, state, zip}}) = "Person named " <> name <> ", aged " <> (show age) <> ", living at " <> street1 <> " " <> street2 <> " " <> city <> ", " <> state <> " " <> zip

checkBuiltinTypeclasses :: Spec Unit
checkBuiltinTypeclasses =
    describe "Eq typeclass" do
        it "is defined for Person" do
            shouldEqual
                (Person
                    { name: "J. B. Rainsberger"
                    , age: 47
                    , address:
                        { street1: "423 Water St"
                        , street2: ""
                        , city: "Summerside"
                        , state: "PE"
                        , zip: "C1N1C8"
                        }
                    })
                (Person
                    { name: "J. B. Rainsberger"
                    , age: 47
                    , address:
                        { street1: "423 Water St"
                        , street2: ""
                        , city: "Summerside"
                        , state: "PE"
                        , zip: "C1N1C8"
                        }
                    })

main :: Effect Unit
main = do
    launchAff_ $ runSpec [ consoleReporter] do
        checkPrelude
        checkDataList
        checkBuiltinTypeclasses