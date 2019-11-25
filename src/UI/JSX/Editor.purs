module Editor where

import Common (cn, cns, (|$))
import React.Basic (JSX)
import React.Basic.DOM (button, div, fieldset, form, h1, input, text, textarea)
import Snap.React.Component ((|-), (|<), (|=))

editorForm :: JSX
editorForm =
  form
  |- fieldset
     |< [ fieldset
          |= cn "form-group"
          |- input
             |= cns ["form-control", "form-control-lg"]
             |$ { placeholder: "Article Title", type: "text" }
        , fieldset
          |= cn "form-group"
          |- input
             |= cn "form-control"
             |$ { placeholder: "What's this article about", type: "text" }
        , fieldset
          |= cn "form-group"
          |- textarea
             |= cn "form-control"
             |$ { placeholder: "Write your article (in markdown)", rows: 8 }
        , fieldset
          |= cn "form-group"
          |< [ input
               |= cn "form-control"
               |$ { placeholder: "Enter tags", type: "text" }
             , div
               |= cn "tag-list"
               |< []
             ]
        , button
          |= cns ["btn", "btn-lg", "pull-xs-right", "btn-primary"]
          |= { type: "button" }
          |- text "Publish Article"
        ]

editorPage :: JSX
editorPage =
  div
  |= cn "editor-page"
  |- div
     |= cns ["container", "page"]
     |- div
        |= cn "row"
        |- div
           |= cns ["col-md-10", "offset-md-1", "col-xs-12"]
           |- editorForm
