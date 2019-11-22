module Nav where

import Prelude

import React.Basic as R
import React.Basic.DOM as R
import Snap.React.Component ((|-), (|<), (|=))

navItem :: Boolean -> String -> R.JSX -> R.JSX
navItem active href content =
  R.li
  |= { className: "nav-item" }
  |- R.a 
     |= { className, href }
     |- content

  where
  className = "nav-link" <> if active then " active" else ""

nav :: R.JSX
nav =
  R.nav
  |= { className: "navbar navbar-light" }
  |- R.div
     |= { className: "container" }
     |< [ R.a 
          |= { className: "navbar-brand", href: "index.html" }
          |- R.text "conduit"
        , R.ul
          |= { className: "nav navbar-nav pull-xs-right" }
          |< [ navItem true "" $ R.text "Home"
             , navItem false "" $ R.i { className: "ion-compose" } <> R.text "&nbsp;New Post"
             , navItem false "" $ R.i { className: "ion-gear-a" } <> R.text "&nbsp;Settings"
             , navItem false "" $ R.text "Sign up"
             ]
        ]
