module Auth where

import Prelude hiding (div)

import Common (cn, cns, href, (|$))
import Data.Array (foldMap)
import React.Basic (JSX)
import React.Basic.DOM (a, button, div, fieldset, form, h1, input, li, p, text, ul)
import Snap.React.Component ((|-), (|<), (|=))

type Field = { type :: String, placeholder :: String }

field :: Field -> JSX
field v =
  fieldset
  |= cn "form-group"
  |- input
     |= cns ["form-control", "form-control-lg"]
     |$ v

loginForm :: Array Field -> JSX
loginForm fs =
  form
  |- (foldMap field fs <> submit)

  where
  submit =
    button
    |= cns ["btn", "btn-lg", "btn-primary", "pull-xs-right"]
    |- text "Sign up"

fields :: Array Field
fields =
  [ { type: "text", placeholder: "Username" }
  , { type: "text", placeholder: "Email" }
  , { type: "password", placeholder: "Password" }
  ]

authPage :: JSX
authPage =
  div |= cn "auth-page"
  |- div |= cns ["container", "page"]
  |- div |= cn "row"
  |- div |= cns ["col-md-6", "offset-md-3", "col-xs-12"]
  |< [ h1 |= cn "text-xs-center" |- text "Sign up"
     , p
       |= cn "text-xs-center"
       |- a |= href "" |- text "Have an account?"

     , ul
       |= cn "error-messages"
       |- li |- text "That email is already taken"

     , loginForm fields
     ]
