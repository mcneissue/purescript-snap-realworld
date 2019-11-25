module Home where

import Prelude hiding (div)

import Article (articlePreview)
import Common (cn, cns, href)
import Model (Article)
import Nav (NavItemState(..), navPills)
import React.Basic (JSX)
import React.Basic.DOM (a, div, h1, p, text)
import Snap.React.Component ((|-), (|<), (|=))

type HomePage =
  { articles :: Array Article
  , tagList :: Array String
  }

banner :: JSX
banner =
  div
  |= cn "banner"
  |- div
     |= cn "container"
     |< [ h1 |= cn "logo-font" |- text "conduit"
        , p |- text "A place to share your knowledge."
        ]

feedToggle :: JSX
feedToggle =
  div
  |= cn "feed-toggle"
  |- navPills
     [ { state: Disabled, url: "", content: text "Your Feed" }
     , { state: Active, url: "", content: text "Global Feed" }
     ]

tagPill :: String -> JSX
tagPill tag =
  a
  |= cns ["tag-pill", "tag-default"]
  |= href "" -- TODO: Figure out what the point of this empty href is (it's there on demo.realworld.io too)
  |- text tag

tagList :: Array String -> JSX
tagList ts =
  div
  |= cn "tag-list"
  |< map tagPill ts

container :: HomePage -> JSX
container h =
  div
  |= cns ["container", "page"]
  |- div
     |= cn "row"
     |- div
        |= cn "col-md-9"
        |< join
           [ [feedToggle]
           , map articlePreview h.articles
           , [tagList h.tagList]
           ]

home :: HomePage -> JSX
home h =
  div
  |= cn "home-page"
  |< [ banner
     , container h
     ]
