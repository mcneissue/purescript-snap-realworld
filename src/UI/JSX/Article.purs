module Article where

import Prelude

import Common (cn, cns, href)
import React.Basic (JSX)
import React.Basic.DOM (a, div, img, span, button, text, i, h1, p)
import Snap.React.Component ((|-), (|<), (|=))
import Types (Article)

articlePreview :: Article -> JSX
articlePreview article =
  div
  |= cn "article-preview"
  |< [ div
       |= cn "article-meta"
       |< [ a
            |= href article.author.url
            |- img { src: article.author.picture }
          , div
            |= cn "info"
            |< [ a
                 |= cn "author"
                 |= href article.author.url
                 |- text article.author.name
               , span
                 |= cn "date"
                 |- text article.date
               ]
          , button
            |= cns [ "btn"
                   , "btn-outline-primary"
                   , "btn-sm"
                   , "pull-xs-right"
                   ]
            |- i (cn "ion-heart") <> text (show article.hearts)
          , a
            |= cn "preview-link"
            |= href article.url
            |< [ h1 |- text article.title
               , p |- text article.description
               , span |- text "Read more..."
               ]
          ]
     ]
