module Record.ToMap where

import Prelude

import Prim.Row as Row
import Prim.RowList (Nil, kind RowList)
import Prim.RowList as RL
import Record as Record
import Type.Prelude (RLProxy(..), SProxy(..), reflectSymbol, class IsSymbol, class RowToList)
import Data.Map (Map)
import Data.Map as M

class ToMap (xs :: RowList) (r :: # Type) a where
  rlToMapImpl :: RLProxy xs -> Record r -> Map String a

instance emptyToMap :: ToMap Nil r a where
  rlToMapImpl _ _ = mempty

instance addToMap ::
  ( IsSymbol x
  , Row.Cons x a r' r
  , ToMap xs r a
  ) => ToMap (RL.Cons x a xs) r a
  where
    rlToMapImpl _ r = M.insert k v m
      where
        k = reflectSymbol prop
        v = Record.get prop r
        m = rlToMapImpl (RLProxy :: RLProxy xs) r
        prop = SProxy :: SProxy x

rlToMap :: forall r xs a
         . RowToList r xs
        => ToMap xs r a
        => Record r
        -> Map String a
rlToMap = rlToMapImpl (RLProxy :: RLProxy xs)