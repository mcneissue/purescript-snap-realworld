module Api.Types where

import Prelude

import Affjax as AX
import Data.Newtype (class Newtype)
import Foreign (MultipleErrors)

newtype Url = Url String

derive instance newtypeUrl :: Newtype Url _

data ApiError = AffjaxError AX.Error | ParseError MultipleErrors

instance showApiError :: Show ApiError where
  show (AffjaxError e) = AX.printError e
  show (ParseError e) = show e