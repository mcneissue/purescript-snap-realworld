module Main where

import Prelude

import Affjax as AX
import Api (GetArticlesParams, getArticles)
import Api.Types (Url(..))
import Control.Monad.Reader (runReaderT)
import Data.Argonaut as J
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class.Console (logShow, log)

testUrl :: Url
testUrl = Url "https://conduit.productionready.io/api"

params :: GetArticlesParams
params =
  { tag: Just "foo"
  , author: Nothing
  , favorited: Nothing
  , limit: Just 1
  , offset: Nothing
  }

main :: Effect Unit
main = void $ launchAff $ do
  result <- runReaderT (getArticles params) { apiUrl: testUrl} 
  case result of
    Right r -> logShow r
    Left e  -> logShow e
