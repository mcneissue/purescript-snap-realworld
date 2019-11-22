module Api where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Filterable (compact)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (ForeignError)
import Model (Article, Profile, Comment)
import Record.ToMap (rlToMap)
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

type GetArticlesParams = 
  { tag :: Maybe String
  , author :: Maybe String
  , favorited :: Maybe String
  , limit :: Maybe Int
  , offset :: Maybe Int
  }

getArticles :: forall m r
            . MonadAsk { apiUrl :: Url | r } m 
           => MonadAff m 
           => GetArticlesParams
           -> m (Either ApiError (Array Article))
getArticles p = parseGet (buildQuery paramList "/articles") (map _.articles <<< parseArticles)
  where
  paramList = compact $ rlToMap $ p # _limit %~ map show
                                    # _offset %~ map show
  _limit = prop (SProxy :: SProxy "limit")
  _offset = prop (SProxy :: SProxy "offset")

  parseArticles :: String -> Either ApiError { articles :: Array Article }
  parseArticles = readJson

getArticle :: forall m r
            . MonadAsk { apiUrl :: Url | r } m
           => MonadAff m
           => String
           -> m (Either ApiError Article)
getArticle slug = parseGet ("/article/" <> slug) readJson

getProfile :: forall m r
            . MonadAsk { apiUrl :: Url | r } m
           => MonadAff m
           => String
           -> m (Either ApiError Profile)
getProfile username = parseGet ("/profiles/" <> username) readJson

getComments :: forall m r
             . MonadAsk { apiUrl :: Url | r } m
            => MonadAff m
            => String
            -> m (Either ApiError (Array Comment))
getComments slug = parseGet ("/articles/" <> slug <> "/comments") (map _.comments <<< parseComments)
  where
  parseComments :: String -> Either ApiError { comments :: Array Comment }
  parseComments = readJson

getTags :: forall m r
         . MonadAsk { apiUrl :: Url | r } m
        => MonadAff m
        => m (Either ApiError (Array String))
getTags = parseGet "/tags" (map _.tags <<< parseTags)
  where
  parseTags :: String -> Either ApiError { tags :: Array String }
  parseTags = readJson

buildQuery :: Map String String -> String -> String
buildQuery params url | Map.isEmpty params = url
                      | otherwise = url <> "?" <> foldMapWithIndex go unfolded
  where
  unfolded :: Array (Tuple String String)
  unfolded = Map.toUnfoldable params

  go 0 (Tuple k v) = k <> "=" <> v
  go n (Tuple k v) = "&" <> k <> "=" <> v

readJson :: forall a. JSON.ReadForeign a => String -> Either ApiError a
readJson = lmap ParseError <<< JSON.readJSON