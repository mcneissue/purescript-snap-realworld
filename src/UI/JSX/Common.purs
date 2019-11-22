module Common where

import Prelude

import Data.Foldable (intercalate)

cn :: String -> { className :: String }
cn = { className: _ }

cns :: Array String -> { className :: String }
cns = cn <<< intercalate " "

href :: String -> { href :: String }
href = { href: _ }
