module InteractiveData.Core.Variant.Extract
  ( class ExtractVariant
  , extractVariant
  , class ExtractVariantRL
  , extractVariantRL
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import InteractiveData.Core.Types (DataPathSegment(..), Opt, scopeOpt)
import MVC.Variant.Types (VariantState(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
--- ExtractVariant
--------------------------------------------------------------------------------

class
  ExtractVariant (extracts :: Row Type) (rsta :: Row Type) (r :: Row Type)
  | extracts -> rsta r
  where
  extractVariant :: Record extracts -> VariantState rsta -> Opt (Variant r)

instance
  ( ExtractVariantRL rl extracts rsta r
  , RowToList extracts rl
  ) =>
  ExtractVariant extracts rsta r
  where
  extractVariant :: Record extracts -> VariantState rsta -> Opt (Variant r)
  extractVariant extracts (VariantState va) =
    extractVariantRL prxRl extracts va

    where
    prxRl :: Proxy rl
    prxRl = Proxy

--------------------------------------------------------------------------------
--- ExtractVariantRL
--------------------------------------------------------------------------------

class
  ExtractVariantRL
    (rl :: RowList Type)
    (extracts :: Row Type)
    (rsta :: Row Type)
    (r :: Row Type)
  | rl extracts -> rsta r
  where
  extractVariantRL :: Proxy rl -> Record extracts -> Variant rsta -> Opt (Variant r)

instance ExtractVariantRL RL.Nil extracts () r
  where
  extractVariantRL :: Proxy RL.Nil -> Record extracts -> Variant () -> Opt (Variant r)
  extractVariantRL _ _ = V.case_

instance
  ( ExtractVariantRL rl' extracts rsta' r
  , Row.Cons sym (sta -> Opt a) extractsx extracts
  , Row.Cons sym sta rsta' rsta
  , Row.Cons sym a rx r
  , IsSymbol sym
  ) =>
  ExtractVariantRL (RL.Cons sym x rl') extracts rsta r
  where
  extractVariantRL :: Proxy (RL.Cons sym x rl') -> Record extracts -> Variant rsta -> Opt (Variant r)
  extractVariantRL _ extracts =
    tail
      # V.on prxSym head

    where
    head :: sta -> Opt (Variant r)
    head = extract >>> map (V.inj prxSym)

    pathSeg :: DataPathSegment
    pathSeg = SegCase (reflectSymbol prxSym)

    extract :: sta -> Opt a
    extract = Record.get prxSym extracts >>> scopeOpt pathSeg

    tail :: Variant rsta' -> Opt (Variant r)
    tail = extractVariantRL prxRl' extracts

    prxRl' :: Proxy rl'
    prxRl' = Proxy

    prxSym :: Proxy sym
    prxSym = Proxy
