module Api.Types where

import Prelude

import Affjax as AX
import Data.Newtype (class Newtype)
import Foreign (MultipleErrors)
import Simple.JSON (class ReadForeign)

newtype Url = Url String

derive instance newtypeUrl :: Newtype Url _
derive newtype instance readForeignUrl :: ReadForeign Url

data ApiError = AffjaxError AX.Error | ParseError MultipleErrors

instance showApiError :: Show ApiError where
  show (AffjaxError e) = AX.printError e
  show (ParseError e) = show e