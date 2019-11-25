module Profile where

import Prelude hiding (div)

import Article (articlePreview)
import Common (cn, cns, (|$))
import Model (Profile, Article)
import Nav (NavItemState(..), navPills)
import React.Basic (JSX)
import React.Basic.DOM (button, div, h4, i, img, p, text)
import Snap.React.Component ((|-), (|<), (|=))

userInfo :: Profile -> JSX
userInfo profile =
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
                |$ { src: profile.image }
              , h4
                |- text profile.username
              , p
                |- text profile.bio
              , button
                |= cns ["btn", "btn-sm", "btn-outline-secondary", "action-btn"]
                |- i (cn "ion-plus-round") <> text ("&nbsp; Follow " <> profile.username)
              ]

articlesToggle :: JSX
articlesToggle =
  div
  |= cn "articles-toggle"
  |- navPills
     [ { state: Active, url: "", content: text "My Articles" }
     , { state: Inactive, url: "", content: text "Favorited Articles" }
     ]

profileArticles :: Array Article -> JSX
profileArticles articles =
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

type ProfilePage = { authored :: Array Article, favorited :: Array Article, profile :: Profile  }

profilePage :: ProfilePage -> JSX
profilePage pp =
  div
  |= cn "profile-page"
  |< [ userInfo pp.profile
     , profileArticles pp.authored
     ]
