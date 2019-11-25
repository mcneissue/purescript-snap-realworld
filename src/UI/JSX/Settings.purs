module Settings where

import Auth ((|$))
import Common (cn, cns)
import React.Basic (JSX)
import React.Basic.DOM (button, div, fieldset, form, h1, input, text, textarea)
import Snap.React.Component ((|-), (|<), (|=))

settingsForm :: JSX
settingsForm =
  form
  |- fieldset
     |< [ fieldset
          |= cn "form-group"
          |- input
             |= cn "form-control"
             |$ { placeholder: "URL of profile picture", type: "text" }
        , fieldset
          |= cn "form-group"
          |- input
             |= cns ["form-control", "form-control-lg"]
             |$ { placeholder: "Your Name", type: "text" }
        , fieldset
          |= cn "form-group"
          |- textarea
             |= cns ["form-control", "form-control-lg"]
             |$ { placeholder: "Short bio about you", rows: 8 }
        , fieldset
          |= cn "form-group"
          |- input
             |= cns ["form-control", "form-control-lg"]
             |$ { placeholder: "Email", type: "text" }
        , fieldset
          |= cn "form-group"
          |- input
             |= cns ["form-control", "form-control-lg"]
             |$ { placeholder: "Password", type: "password" }
        , button
          |= cns ["btn", "btn-lg", "btn-primary", "pull-xs-right"]
          |- text "Update Settings"
        ]

settingsPage :: JSX
settingsPage =
  div
  |= cn "settings-page"
  |- div
     |= cns ["container", "page"]
     |- div
        |= cn "row"
        |- div
           |= cns ["col-md-6", "offset-md-3", "col-xs-12"]
           |< [ h1 |= cn "text-xs-center" |- text "Your Settings"
              , settingsForm
              ]
