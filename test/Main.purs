module Test.Main where

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Test.Api (Config, TestM, apiSpec, readConfig, runTestM)
import Test.Spec (hoistSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

main :: Effect Unit
main = launchAff_ do
  cfg <- readConfig
  join $ runSpecT defaultConfig [consoleReporter] $ hoistSpec (\x -> x) (\_ -> runTestMAff cfg) apiSpec

runTestMAff :: Config -> TestM ~> Aff
runTestMAff cfg tm = runTestM tm cfg 
                 >>= either (liftEffect <<< throwException) pure