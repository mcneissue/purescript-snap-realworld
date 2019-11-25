module Types where

import Data.DateTime (DateTime)

type Url = String

type Profile = { url :: Url, picture :: Url, name :: String }

type Comment =
  { id :: Int
  , createdAt :: DateTime
  , updatedAt :: DateTime
  , body :: String
  , author :: Profile
  }

type Article =
  { author :: Profile
  , date :: String
  , hearts :: Int
  , title :: String
  , description :: String
  , url :: Url
  , body :: String
  }

type Tag = { name :: String, url :: Url }
