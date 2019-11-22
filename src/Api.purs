module Api where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (ForeignError)
import Model (Article)
import Simple.JSON as JSON

newtype Url = Url String

derive instance newtypeUrl :: Newtype Url _

data ApiError = AffjaxError AX.Error | ParseError (NonEmptyList ForeignError)

instance showApiError :: Show ApiError where
  show (AffjaxError e) = AX.printError e
  show (ParseError e) = show e

get :: forall m r a
     . MonadAsk { apiUrl :: Url | r } m 
    => MonadAff m 
    => ResponseFormat a 
    -> String 
    -> m (Either AX.Error (AX.Response a))
get rf endpoint = do
  root <- asks _.apiUrl
  liftAff $ AX.get rf (unwrap root <> endpoint)

parseGet :: forall m r a
          . MonadAsk { apiUrl :: Url | r } m
         => MonadAff m
         => String
         -> (String -> Either ApiError a)
         -> m (Either ApiError a)
parseGet endpoint parser = map (_ >>= parser <<< _.body) $ lmap AffjaxError <$> get ResponseFormat.string endpoint

getArticles :: forall m r
            . MonadAsk { apiUrl :: Url | r } m 
           => MonadAff m 
           => m (Either ApiError (Array Article))
getArticles = parseGet "/articles/"  (map _.articles <<< articlesParser)

articlesParser :: String -> Either ApiError { articles :: Array Article }
articlesParser = readJson

readJson :: forall a. JSON.ReadForeign a => String -> Either ApiError a
readJson = lmap ParseError <<< JSON.readJSON