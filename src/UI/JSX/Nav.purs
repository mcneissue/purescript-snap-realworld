module Nav where

import Prelude

import Common (cn, cns, href)
import React.Basic (JSX)
import React.Basic.DOM (a, div, i, li, nav, text, ul)
import Snap.React.Component ((|-), (|<), (|=))

data NavItemState = Active | Inactive | Disabled

type NavItem = { state :: NavItemState, url :: String, content :: JSX }

navItem :: NavItem -> JSX
navItem { state, url, content } =
  li
  |= cn "nav-item"
  |- a
     |= cns classes
     |= href url
     |- content

  where
  classes = ["nav-link"] <> case state of
    Active -> ["active"]
    Inactive -> []
    Disabled -> ["disabled"]

navPills :: Array NavItem -> JSX
navPills items =
  ul
  |= cns ["nav", "nav-pills", "outline-active"]
  |< map navItem items

navbar :: JSX
navbar =
  nav
  |= cn "navbar navbar-light"
  |- div
     |= cn "container"
     |< [ a
          |= cn "navbar-brand" |= href "index.html"
          |- text "conduit"
        , ul
          |= cn "nav navbar-nav pull-xs-right"
          |< map navItem items
        ]
  where
  items =
    [ { state: Active, url: "", content: text "Home" }
    , { state: Inactive, url: "", content: i (cn "ion-compose") <> text "&nbsp;New Post" }
    , { state: Inactive, url: "", content: i (cn "ion-gear-a") <> text "&nbsp;Settings" }
    , { state: Inactive, url: "", content: text "Sign up" }
    ]
