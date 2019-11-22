module Main where

import Prelude

import Affjax as AX
import Api (getArticles, Url(..))
import Control.Monad.Reader (runReaderT)
import Data.Argonaut as J
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class.Console (logShow, log)

testUrl :: Url
testUrl = Url "https://conduit.productionready.io/api"

main :: Effect Unit
main = void $ launchAff $ do
  result <- runReaderT getArticles { apiUrl: testUrl} 
  case result of
    Right r -> logShow r
    Left e  -> logShow e
