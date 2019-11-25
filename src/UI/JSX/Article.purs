module Article where

import Prelude hiding (div)

import Common (cn, cns, href, (|$))
import Data.Array (foldMap)
import Model (Article, Comment, Profile, articleUrl, profileUrl)
import React.Basic (JSX)
import React.Basic.DOM (a, button, div, form, h1, hr, i, img, p, span, text, textarea)
import Snap.React.Component ((|-), (|<), (|=))

articleMeta :: Article -> Array JSX -> JSX
articleMeta article btns =
  div
  |= cn "article-meta"
  |< (metaContent <> btns)

  where
  metaContent =
    [ a
      |= href (profileUrl article.author)
      |- img { src: article.author.image }
    , div
      |= cn "info"
      |< [ a
           |= cn "author"
           |= href (profileUrl article.author)
           |- text article.author.username
         , span
           |= cn "date"
           |- text (show article.createdAt)
         ]
    ]

articlePreview :: Article -> JSX
articlePreview article =
  div
  |= cn "article-preview"
  |< [ articleMeta article btns
     , a
       |= cn "preview-link"
       |= href (articleUrl article)
       |< [ h1 |- text article.title
          , p |- text article.description
          , span |- text "Read more..."
          ]
     ]
  where
  btns = [fave]
  fave =
    button
    |= cns ["btn", "btn-sm", "btn-outline-primary", "pull-xs-right"]
    |- i (cn "ion-heart") <> text (show article.favoritesCount)

followButton :: Profile -> JSX
followButton p =
  button
  |= cns ["btn", "btn-sm", "btn-outline-secondary"]
  |< [ i |$ cn "ion-plus-round"
     , text ("&nbsp; Follow " <> p.username)
     ]

favoriteButton :: Article -> JSX
favoriteButton a =
  button
  |= cns ["btn", "btn-sm", "btn-outline-primary"]
  |< [ i |$ cn "ion-heart"
     , text "&nbsp; Favorite Post "
     , span
       |= cn "counter"
       |- text ("(" <> show a.favoritesCount <> ")")
     ]

banner :: Article -> JSX
banner article =
  div
  |= cn "banner"
  |- div
     |= cn "container"
     |< [ h1 |- text "How to build webapps that scale"
        , articleMeta
            article
            [ followButton article.author
            , text "&nbsp;&nbsp;"
            , favoriteButton article
            ]
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
  |- articleMeta
       article
       [ followButton article.author
       , text "&nbsp;&nbsp;"
       , favoriteButton article
       ]

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
            |= href (profileUrl c.author)
            |- img
               |= cn "comment-author-img"
               |$ { src: c.author.image }
          ]
     ]

commentSection :: Array Comment -> JSX
commentSection cs =
  div
  |= cn "row"
  |- div
     |= cns ["col-xs-12", "col-md-8", "offset-md-2"]
     |- commentForm <> foldMap comment cs

type ArticlePage =
  { article :: Article
  , comments :: Array Comment
  }

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
