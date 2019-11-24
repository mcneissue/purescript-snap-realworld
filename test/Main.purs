module Test.Main where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_, Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throwException)
import Snap.SYTC.Component (identity)
import Test.Api (TestM(..), apiSpec, readConfig, runTestM, Config)
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