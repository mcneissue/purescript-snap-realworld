module FakeData where

import Prelude

import Data.Date (Date, Month(..), exactDate)
import Data.DateTime (DateTime(..), Time)
import Data.Enum (toEnum)
import Data.Maybe (Maybe)
import Model (Profile, Article)
import Unsafe.Coerce (unsafeCoerce)

profiles :: { eric :: Profile, albert :: Profile }
profiles =
  { eric:   { image: "http://i.imgur.com/Qr71crq.jpg", username: "Eric Simons", bio: "", following: true }
  , albert: { image: "http://i.imgur.com/N4VcUeJ.jpg", username: "Albert Pai", bio: "", following: false }
  }

fromMaybe :: forall a. Maybe a -> a
fromMaybe = unsafeCoerce

jan20 :: Date
jan20 = fromMaybe $ do
  y <- toEnum 2019
  d <- toEnum 20
  exactDate y January d

midnight :: Time
midnight = bottom

articles :: Array Article
articles =
  [ { slug: "article-1"
    , title: "How to build webapps that scale"
    , description: "This is the description for the post."
    , body: "\
            \<p>\
            \  Web development technologies have evolved at an incredible clip\
            \  over the past few years.\
            \</p>\
            \<h2 id=\"introducing-ionic\">Introducing RealWorld.</h2>\
            \<p>It's a great solution for learning how other frameworks work.</p>\
            "
    , tagList: []
    , createdAt: DateTime jan20 midnight
    , updatedAt: DateTime jan20 midnight
    , favorited: true
    , favoritesCount: 29
    , author: profiles.eric
    }

  , { slug: "article-2"
    , title: "The song you won't ever stop singing. No matter how hard you try"
    , description: "This is the description for the post"
    , body: "\
            \<p>\
            \  Look at me I am say things too\
            \</p>\
            "
    , tagList: []
    , createdAt: DateTime jan20 midnight
    , updatedAt: DateTime jan20 midnight
    , favorited: false
    , favoritesCount: 32
    , author: profiles.albert
    }
  ]

tagList :: Array String
tagList = ["programming", "javascript", "emberjs", "angularjs", "react", "mean", "node", "rails"]
