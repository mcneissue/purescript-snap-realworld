module Model where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Data.DateTime (DateTime) as DT
import Data.DateTime.Instant (instant, toDateTime)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Time.Duration (Milliseconds(..))
import Foreign (ForeignError(..), readString)
import Simple.JSON (class ReadForeign)

foreign import parseDateMs :: String -> Number

newtype DateTime = DateTime DT.DateTime

derive instance newtypeDateTime :: Newtype DateTime _
derive newtype instance showDateTime :: Show DateTime

instance readForeignDateTime :: ReadForeign DateTime where
  readImpl t = go >>= maybeThrow (pure $ ForeignError "instant failed to parse Milliseconds")
    where 
    go = map (wrap <<< toDateTime) 
      <<< instant 
      <<< Milliseconds 
      <<< parseDateMs 
      <$> readString t

maybeThrow :: forall e m a. Monad m => e -> Maybe a -> ExceptT e m a
maybeThrow e = maybe (throwError e) pure

newtype Token = Token String

derive instance newtypeToken :: Newtype Token _
derive newtype instance readForeignToken :: ReadForeign Token

type BaseArticle r = 
  { title :: String
  , description :: String
  , body :: String
  , tagList :: Array String
  | r
  }

type CreateArticle = BaseArticle ()
type UpdateArticle = BaseArticle ()

type Article = BaseArticle 
  ( slug :: String
  , createdAt :: DateTime
  , updatedAt :: DateTime
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: Profile
  )

type Comment =
  { id :: Int
  , createdAt :: DateTime
  , updatedAt :: DateTime
  , body :: String
  , author :: Profile
  }

type Profile = 
  { username :: String
  , bio :: Maybe String
  , image :: String
  , following :: Boolean
  }

type BaseUser r =
  { email :: String
  , username :: String
  , bio :: String
  , image :: String
  | r
  }

type UpdateUser = BaseUser (password :: Maybe String)

type User = BaseUser (token :: Token)
  
