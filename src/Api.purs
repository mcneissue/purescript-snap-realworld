module Api where

import Prelude

import Affjax as AX
import Affjax.RequestBody (RequestBody)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Filterable (compact)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (MultipleErrors)
import Model (CreateArticle, Article, Comment, Profile, Token, User, UpdateUser, UpdateArticle)
import Prim.Row as Row
import Record as Record
import Record.ToMap (rlToMap)
import Simple.JSON as JSON

newtype Url = Url String

derive instance newtypeUrl :: Newtype Url _

data ApiError = AffjaxError AX.Error | ParseError MultipleErrors

instance showApiError :: Show ApiError where
  show (AffjaxError e) = AX.printError e
  show (ParseError e) = show e

request :: forall m a
         . MonadAff m
        => AX.Request a
        -> m (Either ApiError (AX.Response a))
request = map (lmap AffjaxError) <<< liftAff <<< AX.request

authReq :: forall m r a
         . MonadAsk { apiUrl :: Url | r } m
        => MonadAff m
        => Either Method CustomMethod
        -> Maybe RequestBody
        -> Maybe Token
        -> ResponseFormat a
        -> String
        -> m (Either ApiError (AX.Response a))
authReq method mcontent mt rf url = do
  root <- asks (_.apiUrl >>> unwrap)
  let req = maybe AX.defaultRequest defaultAuthReq mt
  request $ req 
    { url = root <> url
    , responseFormat = rf
    , method = method
    , content = mcontent 
    }

defaultAuthReq :: Token -> AX.Request Unit
defaultAuthReq token = AX.defaultRequest 
  { headers = [ RequestHeader "Token" (unwrap token) ]
  }

parseAuthMethod :: forall m r a
                 . MonadAsk { apiUrl :: Url | r } m
                => MonadAff m
                => Either Method CustomMethod
                -> Maybe Token
                -> String
                -> Maybe RequestBody
                -> (String -> Either ApiError a)
                -> m (Either ApiError a)
parseAuthMethod method mt endpoint body parser =
  map (_ >>= parser <<< _.body) $ authReq method body mt ResponseFormat.string endpoint

parseAuthGet :: forall m r a
              . MonadAsk { apiUrl :: Url | r } m
             => MonadAff m
             => Maybe Token
             -> String
             -> (String -> Either ApiError a)
             -> m (Either ApiError a)
parseAuthGet t s p = parseAuthMethod (Left GET) t s Nothing p

parseGet :: forall m r a
          . MonadAsk { apiUrl :: Url | r } m
         => MonadAff m
         => String
         -> (String -> Either ApiError a)
         -> m (Either ApiError a)
parseGet = parseAuthGet Nothing

parseAuthPost :: forall m r a
               . MonadAsk { apiUrl :: Url | r } m
              => MonadAff m
              => Maybe Token
              -> String
              -> Maybe RequestBody
              -> (String -> Either ApiError a)
              -> m (Either ApiError a)
parseAuthPost = parseAuthMethod $ Left POST

parsePost :: forall m r a
           . MonadAsk { apiUrl :: Url | r } m
          => MonadAff m
          => String
          -> Maybe RequestBody
          -> (String -> Either ApiError a)
          -> m (Either ApiError a)
parsePost = parseAuthPost Nothing

parseAuthPut :: forall m r a
              . MonadAsk { apiUrl :: Url | r } m
             => MonadAff m
             => Maybe Token
             -> String
             -> Maybe RequestBody
             -> (String -> Either ApiError a)
             -> m (Either ApiError a)
parseAuthPut = parseAuthMethod $ Left PUT

parseAuthDelete :: forall m r a
                 . MonadAsk { apiUrl :: Url | r } m
                => MonadAff m
                => Maybe Token
                -> String
                -> (String -> Either ApiError a)
                -> m (Either ApiError a)
parseAuthDelete t s p = parseAuthMethod (Left DELETE) t s Nothing p

getCurrentUser :: forall m r
                . MonadAsk { apiUrl :: Url | r } m
               => MonadAff m
               => Token
               -> m (Either ApiError User)
getCurrentUser t =
  parseAuthGet (Just t)
    "/users"
    (readJson' (SProxy :: SProxy "user"))

type GetFeedParams =
  { limit :: Maybe Int
  , offset :: Maybe Int
  }

getFeed :: forall m r
         . MonadAsk { apiUrl :: Url | r } m
        => MonadAff m
        => Token
        -> GetFeedParams
        -> m (Either ApiError (Array Article))
getFeed t p =
  parseAuthGet (Just t)
    (buildQuery paramList "/articles/feed")
    (readJson' _articles)
  where
  paramList = compact $ rlToMap $ p # prop _limit %~ map show
                                    # prop _offset %~ map show

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
getArticles p = 
  parseGet 
    (buildQuery paramList "/articles") 
    (readJson' _articles)
  where
  paramList = compact $ rlToMap $ p # prop _limit %~ map show
                                    # prop _offset %~ map show

getArticle :: forall m r
            . MonadAsk { apiUrl :: Url | r } m
           => MonadAff m
           => String
           -> m (Either ApiError Article)
getArticle slug = 
  parseGet 
    ("/article/" <> slug) 
    (readJson' _article)

getProfile :: forall m r
            . MonadAsk { apiUrl :: Url | r } m
           => MonadAff m
           => String
           -> m (Either ApiError Profile)
getProfile username = 
  parseGet 
    ("/profiles/" <> username) 
    (readJson' _profile)

getComments :: forall m r
             . MonadAsk { apiUrl :: Url | r } m
            => MonadAff m
            => String
            -> m (Either ApiError (Array Comment))
getComments slug = 
  parseGet 
    ("/articles/" <> slug <> "/comments") 
    (readJson' (SProxy :: SProxy "comments"))

deleteComment :: forall m r
               . MonadAsk { apiUrl :: Url | r } m
              => MonadAff m
              => Token
              -> String
              -> Int
              -> m (Either ApiError Unit)
deleteComment token slug id =
  parseAuthDelete
    (Just token)
    ("/articles/" <> slug <> "/comments/" <> show id)
    (const $ Right unit)

getTags :: forall m r
         . MonadAsk { apiUrl :: Url | r } m
        => MonadAff m
        => m (Either ApiError (Array String))
getTags = 
  parseGet 
    "/tags" 
    (readJson' (SProxy :: SProxy "tags"))

postLogin :: forall m r
           . MonadAsk { apiUrl :: Url | r } m
          => MonadAff m
          => { username :: String, password :: String }
          -> m (Either ApiError User)
postLogin login =
  parsePost
    "/users/login"
    (Just $ mkJsonBody { user: login })
    (readJson' _user)

postRegister :: forall m r
              . MonadAsk { apiUrl :: Url | r } m
             => MonadAff m
             => { username :: String, email :: String, password :: String }
             -> m (Either ApiError User)
postRegister register =
  parsePost
    "/users"
    (Just $ mkJsonBody { user: register })
    (readJson' _user)

postFollow :: forall m r
            . MonadAsk { apiUrl :: Url | r } m
           => MonadAff m
           => Token
           -> String
           -> m (Either ApiError Profile)
postFollow token username =
  parseAuthPost
    (Just token)
    ("/profiles/" <> username <> "/follow")
    Nothing
    (readJson' _profile)

postArticle :: forall m r
             . MonadAsk { apiUrl :: Url | r } m
            => MonadAff m
            => Token
            -> CreateArticle
            -> m (Either ApiError Article)
postArticle token article =
  parseAuthPost
    (Just token)
    "/articles"
    (Just $ mkJsonBody { article })
    (readJson' _article)

deleteArticle :: forall m r
               . MonadAsk { apiUrl :: Url | r } m
              => MonadAff m
              => Token
              -> String
              -> m (Either ApiError Unit)
deleteArticle token slug = 
  parseAuthDelete
    (Just token)
    ("/articles/" <> slug)
    (const $ Right unit)

postFavoriteArticle :: forall m r
                     . MonadAsk { apiUrl :: Url | r } m
                    => MonadAff m
                    => Token
                    -> String
                    -> m (Either ApiError Article)
postFavoriteArticle token slug =
  parseAuthPost
    (Just token)
    ("/articles/" <> slug <> "/favorite")
    Nothing
    (readJson' _article)

deleteFavoriteArticle :: forall m r
                       . MonadAsk { apiUrl :: Url | r } m
                      => MonadAff m
                      => Token
                      -> String
                      -> m (Either ApiError Article)
deleteFavoriteArticle token slug =
  parseAuthDelete
    (Just token)
    ("/articles/" <> slug <> "/favorite")
    (readJson' _article)

postComment :: forall m r
             . MonadAsk { apiUrl :: Url | r } m
            => MonadAff m
            => Token
            -> String
            -> { body :: String }
            -> m (Either ApiError Comment)
postComment token slug comment =
  parseAuthPost
    (Just token)
    ("/articles/" <> slug <> "/comments")
    (Just $ mkJsonBody { comment })
    (readJson' _comment)

putUser :: forall m r
         . MonadAsk { apiUrl :: Url | r } m
        => MonadAff m
        => Token
        -> UpdateUser
        -> m (Either ApiError User)
putUser token user =
  parseAuthPut
    (Just token)
    "/user"
    (Just $ mkJsonBody { user })
    (readJson' _user)

putArticle :: forall m r
            . MonadAsk { apiUrl :: Url | r } m
           => MonadAff m
           => Token
           -> String
           -> UpdateArticle
           -> m (Either ApiError User)
putArticle token slug article =
  parseAuthPut
    (Just token)
    ("/articles/" <> slug)
    (Just $ mkJsonBody { article })
    (readJson' _article)

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

-- Helper for unwrapping responses like { foo :: Foo }
readJson' :: forall l r a. JSON.ReadForeign a => JSON.ReadForeign (Record r) => IsSymbol l => Row.Cons l a () r => SProxy l -> String -> Either ApiError a
readJson' _ s = bimap ParseError (Record.get (SProxy :: SProxy l)) $ (JSON.readJSON s :: Either MultipleErrors (Record r))

mkJsonBody :: forall a. JSON.WriteForeign a => a -> RequestBody
mkJsonBody = RequestBody.string <<< JSON.writeJSON

_user :: SProxy "user"
_user = SProxy

_profile :: SProxy "profile"
_profile = SProxy

_articles :: SProxy "articles"
_articles = SProxy

_article :: SProxy "article"
_article = SProxy

_limit :: SProxy "limit"
_limit = SProxy

_offset :: SProxy "offset"
_offset = SProxy

_comment :: SProxy "comment"
_comment = SProxy