module Nav where

import Prelude

import Common (cn, href)
import React.Basic (JSX)
import React.Basic.DOM (a, div, i, li, nav, text, ul)
import Snap.React.Component ((|-), (|<), (|=))

navItem :: Boolean -> String -> JSX -> JSX
navItem active href content =
  li
  |= cn "nav-item"
  |- a
     |= { className, href }
     |- content

  where
  className = "nav-link" <> if active then " active" else ""

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
          |< [ navItem true "" $ text "Home"
             , navItem false "" $ i (cn "ion-compose") <> text "&nbsp;New Post"
             , navItem false "" $ i (cn "ion-gear-a") <> text "&nbsp;Settings"
             , navItem false "" $ text "Sign up"
             ]
        ]
