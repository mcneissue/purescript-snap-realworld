module Model where

import Prelude
import Data.DateTime (DateTime)

type Article =
  { slug :: String
  , title :: String
  , description :: String
  , body :: String
  , tagList :: Array String
  , createdAt :: DateTime
  , updatedAt :: DateTime
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: User
  }

type Comment =
  { id :: Int
  , createdAt :: DateTime
  , updatedAt :: DateTime
  , body :: String
  , author :: User
  }

type Profile = 
  { username :: String
  , bio :: String
  , image :: String
  , following :: Boolean
  }

type User =
  { email :: String
  , token :: String
  , username :: String
  , bio :: String
  , image :: String
  }
