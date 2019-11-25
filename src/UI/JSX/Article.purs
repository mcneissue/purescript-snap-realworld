module Article where

import Prelude

import Common (cn, cns, href, (|$))
import Data.Array (foldMap)
import Data.DateTime (DateTime(..))
import React.Basic (JSX)
import React.Basic.DOM (a, button, div, form, h1, hr, i, img, p, span, text, textarea)
import Snap.React.Component ((|-), (|<), (|=))
import Types (Article, Profile, Comment)

articleMeta :: Article -> Array JSX -> JSX
articleMeta article buttons =
  div
  |= cn "article-meta"
  |< (metaContent <> buttons)

  where
  metaContent =
    [ a
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
    ]

articlePreview :: Article -> JSX
articlePreview article =
  div
  |= cn "article-preview"
  |< [ articleMeta article buttons
     , a
       |= cn "preview-link"
       |= href article.url
       |< [ h1 |- text article.title
          , p |- text article.description
          , span |- text "Read more..."
          ]
     ]
  where
  buttons = [fave]
  fave =
    button
    |= cns ["btn", "btn-sm", "btn-outline-primary", "pull-xs-right"]
    |- i (cn "ion-heart") <> text (show article.hearts)

buttons :: Article -> Array JSX
buttons article = [follow, spacer, fave]
  where
  spacer = text "&nbsp;&nbsp;"
  follow =
    button
    |= cns ["btn", "btn-sm", "btn-outline-secondary"]
    |< [ i |$ cn "ion-plus-round"
       , text ("&nbsp; Follow " <> article.author.name)
       ]
  fave =
    button
    |= cns ["btn", "btn-sm", "btn-outline-primary"]
    |< [ i |$ cn "ion-heart"
       , text "&nbsp; Favorite Post "
       , span
         |= cn "counter"
         |- text ("(" <> show article.hearts <> ")")
       ]

banner :: Article -> JSX
banner article =
  div
  |= cn "banner"
  |- div
     |= cn "container"
     |< [ h1 |- text "How to build webapps that scale"
        , articleMeta article (buttons article)
        ]

articleContent :: Article -> JSX
articleContent a =
  div
  |= cns ["row", "article-content"]
  |- div
     |= cn "col-md-12"
     |$ { dangerouslySetInnerHTML: { __html: a.body } }

articleActions :: Article -> JSX
articleActions article =
  div
  |= cns ["article-actions"]
  |- articleMeta article (buttons article)

commentForm :: JSX
commentForm =
  form
  |= cns ["card", "comment-form"]
  |< [ div
       |= cn "card-block"
       |- textarea
          |= cn "form-control"
          |$ { placeholder: "Write a comment...", rows: 3 }
     , div
       |= cn "card-footer"
       |< [ img
            |= cn "comment-author-img"
            |$ { src: "http://i.imgur.com/Qr71crq.jpg" } -- TODO: Get this from the current user
          , button
            |= cns ["btn", "btn-sm", "btn-primary"]
            |- text "Post Comment"
          ]
     ]

comment :: Comment -> JSX
comment c =
  div
  |= cn "card"
  |< [ div
       |= cn "card-block"
       |- p
          |= cn "card-text"
          |- text c.body
     , div
       |= cn "card-footer"
       |< [ a
            |= cn "comment-author"
            |= href c.author.url
            |- img
               |= cn "comment-author-img"
               |$ { src: c.author.picture }
          ]
     ]

commentSection :: Array Comment -> JSX
commentSection cs =
  div
  |= cn "row"
  |- div
     |= cns ["col-xs-12", "col-md-8", "offset-md-2"]
     |- commentForm <> foldMap comment cs

type ArticlePage = { article :: Article, comments :: Array Comment }
articlePage :: ArticlePage -> JSX
articlePage s =
  div
  |= cn "article-page"
  |- div
     |= cns ["container", "page"]
     |< [ banner s.article
        , articleContent s.article
        , hr {}
        , articleActions s.article
        , commentSection s.comments
        ]
