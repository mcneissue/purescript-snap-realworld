module Home where

import Prelude hiding (div)

import Common (cn, cns, href)
import React.Basic (JSX)
import React.Basic.DOM (a, button, div, h1, i, img, li, p, span, text, ul)
import Snap.React.Component ((|-), (|<), (|=))
import Types (Article, Tag)

type HomePageData = { articles :: Array Article, tags :: Array Tag }

banner :: JSX
banner =
  div |= cn "banner"
  |- div |= cn "container"
     |< [ h1 |= cn "logo-font" |- text "conduit"
        , p |- text "A place to share your knowledge."
        ]

navItem :: Boolean -> String -> JSX -> JSX
navItem active url content =
  li |= cn "nav-item"
  |- a |= cns classes |= href url
     |- content

  where
  classes = ["nav-link"] <> if active then ["active"] else ["disabled"]

feedToggle :: JSX
feedToggle =
  div |= cn "feed-toggle"
  |- ul |= cns ["nav", "nav-pills", "outline-active"]
     |< [ navItem false "" $ text "Your Feed"
        , navItem true  "" $ text "Global Feed"
        ]

-- variant 1
articlePreview :: Article -> JSX
articlePreview article =
  div |= cn "article-preview"
  |< [ div |= cn "article-meta"
       |< [ a |= href article.author.url
            |- img { src: article.author.picture }
          , div |= cn "info"
            |< [ a |= cn "author" |= href article.author.url
                 |- text article.author.name
               , span |= cn "date"
                 |- text article.date
               ]
          , button |= cns ["btn", "btn-outline-primary", "btn-sm", "pull-xs-right"]
            |- i (cn "ion-heart") <> text (show article.hearts)
          , a |= cn "preview-link" |= href article.url
            |< [ h1 |- text article.title
               , p |- text article.description
               , span |- text "Read more..."
               ]
          ]
     ]

tagPill :: Tag -> JSX
tagPill tag =
  a |= cns ["tag-pill", "tag-default"] |= href tag.url
  |- text tag.name

tagList :: Array Tag -> JSX
tagList ts =
  div |= cn "tag-list"
  |< map tagPill ts

container :: HomePageData -> JSX
container hpd =
  div |= cns ["container", "page"]
  |- div |= cn "row"
     |- div |= cn "col-md-9"
        |< ([ feedToggle ] <> map articlePreview hpd.articles <> [ tagList hpd.tags ])

home :: HomePageData -> JSX
home hpd =
  div |= cn "home-page"
  |< [ banner
     , container hpd
     ]
