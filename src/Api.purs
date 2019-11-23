module Api where

import Prelude (Unit, const, map, show, unit, (#), ($), (<>))

import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Data.Filterable (compact)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Model (CreateArticle, Article, Comment, Profile, Token, User, UpdateUser, UpdateArticle)
import Record.ToMap (rlToMap)

import Api.Types (ApiError, Url)
import Api.Util (buildQuery, mkJsonBody, parseAuthDelete, parseAuthGet, parseAuthPost, parseAuthPut, parseGet, parsePost, readJson')

getCurrentUser :: forall m r
                . MonadAsk { apiUrl :: Url | r } m
               => MonadAff m
               => Token
               -> m (Either ApiError User)
getCurrentUser t =
  parseAuthGet (Just t)
    "/users"
    (readJson' _user)

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

defaultGetArticlesParams :: GetArticlesParams
defaultGetArticlesParams = 
  { tag: Nothing
  , author: Nothing
  , favorited: Nothing
  , limit: Nothing
  , offset: Nothing 
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
    (readJson' _comments)

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
    (readJson' _tags)

postLogin :: forall m r
           . MonadAsk { apiUrl :: Url | r } m
          => MonadAff m
          => { email :: String, password :: String }
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

deleteFollow :: forall m r
              . MonadAsk { apiUrl :: Url | r } m
             => MonadAff m
             => Token
             -> String
             -> m (Either ApiError Unit)
deleteFollow token username =
  parseAuthDelete
    (Just token)
    ("/profiles/" <> username <> "/follow")
    (const $ Right unit)

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

_comments :: SProxy "comments"
_comments = SProxy

_tags :: SProxy "tags"
_tags = SProxy