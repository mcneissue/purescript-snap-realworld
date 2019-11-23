module Test.Main where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Test.Api (cleanupServer, readConfig, startServer, testGetArticles, runTestM)

main :: Effect Unit
main = launchAff_ do
  cfg <- readConfig
  er <- runTestM (startServer >>= \s -> (liftAff $ delay (Milliseconds 1000.0)) *> void testGetArticles *> cleanupServer s) cfg
  case er of
    Right _ -> pure unit
    Left e -> liftEffect $ logShow e
