module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.CheckDataList (checkDataList)
import Test.CheckPrelude (checkPrelude)
import Test.CheckSimpleTypeclasses (checkBuiltinTypeclasses)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
    launchAff_ $ runSpec [ consoleReporter] do
        checkPrelude
        checkDataList
        checkBuiltinTypeclasses