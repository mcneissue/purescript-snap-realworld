module Router where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Effect (Effect)
import Routing.Duplex (RouteDuplex', parse, print, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (matchesWith)

data Route
  = Root
  | Login
  | Register
  | Settings
  | NewArticle
  | EditArticle String
  | Article String
  | Profile String
  | Favorites String

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

parser :: RouteDuplex' Route
parser = root $ sum
  { "Root": noArgs
  , "Login": "login" / noArgs
  , "Register": "register" / noArgs
  , "Settings": "settings" / noArgs
  , "NewArticle": "editor" / noArgs
  , "EditArticle": "editor" / segment
  , "Article": "article" / segment
  , "Profile": "profile" / segment
  , "Favorites": "profile" / "favorites" / segment
  }

-- Adds a hashchange listener and returns an Effect that will remove
-- the listener.
mkRouter :: (Maybe Route -> Route -> Effect Unit) -> Effect (Effect Unit)
mkRouter = matchesWith (parse parser)

urlFor :: Route -> String
urlFor r = "/#" <> print parser r