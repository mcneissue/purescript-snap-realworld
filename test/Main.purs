module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Api (runApiSpec)

main :: Effect Unit
main = launchAff_ runApiSpec