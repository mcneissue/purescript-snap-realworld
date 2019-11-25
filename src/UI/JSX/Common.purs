module Common where

import Prelude

import Control.Category (identity)
import Data.Foldable (intercalate)

cn :: String -> { className :: String }
cn = { className: _ }

cns :: Array String -> { className :: String }
cns = cn <<< intercalate " "

href :: String -> { href :: String }
href = { href: _ }

infixl 7 identity as |$
