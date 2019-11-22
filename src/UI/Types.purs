module Types where

type Url = String

type Profile = { url :: Url, picture :: Url, name :: String }

type Article =
  { author :: Profile
  , date :: String
  , hearts :: Int
  , title :: String
  , description :: String
  , url :: Url
  }

type Tag = { name :: String, url :: Url }
