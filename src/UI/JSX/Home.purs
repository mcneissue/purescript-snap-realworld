module Home where

import Prelude hiding (div)

import Article (articlePreview)
import Common (cn, cns, href)
import Nav (NavItemState(..), navPills)
import React.Basic (JSX)
import React.Basic.DOM (a, div, h1, li, p, text, ul)
import Snap.React.Component ((|-), (|<), (|=))
import Types (Article, Tag)

type HomePageData = { articles :: Array Article, tags :: Array Tag }

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

tagPill :: Tag -> JSX
tagPill tag =
  a
  |= cns ["tag-pill", "tag-default"]
  |= href tag.url
  |- text tag.name

tagList :: Array Tag -> JSX
tagList ts =
  div
  |= cn "tag-list"
  |< map tagPill ts

container :: HomePageData -> JSX
container hpd =
  div
  |= cns ["container", "page"]
  |- div
     |= cn "row"
     |- div
        |= cn "col-md-9"
        |< join
           [ [feedToggle]
           , map articlePreview hpd.articles
           , [tagList hpd.tags]
           ]

home :: HomePageData -> JSX
home hpd =
  div
  |= cn "home-page"
  |< [ banner
     , container hpd
     ]
