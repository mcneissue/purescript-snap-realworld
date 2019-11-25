module Model where

import Data.DateTime (DateTime)

type Url = String

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
  , author :: Profile
  }

type Comment =
  { id :: Int
  , createdAt :: DateTime
  , updatedAt :: DateTime
  , body :: String
  , author :: Profile
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

profileUrl :: Profile -> Url
profileUrl _ = "" -- TODO: Replace with this an actual URL getting thing

articleUrl :: Article -> Url
articleUrl _ = ""
