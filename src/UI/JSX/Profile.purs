module Profile where

import Prelude hiding (div)

import Article (articlePreview)
import Common (cn, cns, (|$))
import FakeData (articles)
import Nav (NavItemState(..), navPills)
import React.Basic (JSX)
import React.Basic.DOM (button, div, h4, i, img, p, text)
import Snap.React.Component ((|-), (|<), (|=))

userInfo :: JSX
userInfo =
  div
  |= cn "user-info"
  |- div
     |= cn "container"
     |- div
        |= cn "row"
        |- div
           |= cns ["col-xs-12", "col-md-10", "offset-md-1"]
           |< [ img
                |= cn "user-img"
                |$ { src: "http://i.imgur.com/Qr71crq.jpg" }
              , h4
                |- text "Eric Simons"
              , p
                |- text "Cofounder @GoThinkster, lived in Aol's HQ for a few months,\nkinda looks like Peeta from the Hunger Games"
              , button
                |= cns ["btn", "btn-sm", "btn-outline-secondary", "action-btn"]
                |- (i (cn "ion-plus-round") <> text "&nbsp; Follow Eric Simons")
              ]

articlesToggle :: JSX
articlesToggle =
  div
  |= cn "articles-toggle"
  |- navPills
     [ { state: Active, url: "", content: text "My Articles" }
     , { state: Inactive, url: "", content: text "Favorited Articles" }
     ]

container :: JSX
container =
  div
  |= cn "container"
  |- div
     |= cn "row"
     |- div
        |= cns ["col-xs-13", "col-md-10", "offset-md-1"]
        |< contents
  where
  contents = [articlesToggle] <> articlePreviews
  articlePreviews =
    map articlePreview articles

profilePage :: JSX
profilePage =
  div
  |= cn "profile-page"
  |< [ userInfo
     , container
     ]
