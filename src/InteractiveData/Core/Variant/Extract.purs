module InteractiveData.Core.Variant.Extract where

import Prelude

import Data.Bifunctor (lmap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import InteractiveData.Core.Types (DataPathSegment(..), Opt, scopeError)
import InteractiveData.TestTypes (S1, T1)
import MVC.Variant.Types (VariantState(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class ExtractVariant extracts rsta r | extracts -> rsta r where
  extractVariant :: Record extracts -> VariantState rsta -> Opt (Variant r)

instance
  ( ExtractVariantRL rl extracts rsta r
  , RowToList extracts rl
  ) =>
  ExtractVariant extracts rsta r where
  extractVariant extracts (VariantState va) = extractVariantRL prxRl extracts va
    where
    prxRl = Proxy :: _ rl

---

class ExtractVariantRL :: RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class ExtractVariantRL rl extracts rsta r | rl extracts -> rsta r where
  extractVariantRL :: Proxy rl -> Record extracts -> Variant rsta -> Opt (Variant r)

instance ExtractVariantRL RL.Nil extracts () r where
  extractVariantRL _ _ = V.case_

instance
  ( ExtractVariantRL rl' extracts rsta' r
  , Row.Cons sym (sta -> Opt a) extractsx extracts
  , Row.Cons sym sta rsta' rsta
  , Row.Cons sym a rx r
  , IsSymbol sym
  ) =>
  ExtractVariantRL (RL.Cons sym x rl') extracts rsta r where
  extractVariantRL _ extracts =
    tail
      # V.on prxSym head
    where
    head :: sta -> Opt (Variant r)
    head = extract >>> map (V.inj prxSym)

    pathSeg :: DataPathSegment
    pathSeg = SegCase (reflectSymbol prxSym)

    extract :: sta -> Opt a
    extract = Record.get prxSym extracts >>> lmap (scopeError pathSeg)

    tail :: Variant rsta' -> Opt (Variant r)
    tail = extractVariantRL prxRl' extracts

    prxRl' = Proxy :: _ rl'
    prxSym = Proxy :: _ sym

---

testExtractVariant
  :: Record
       ( field1 :: S1 -> Opt T1
       , field2 :: S1 -> Opt T1
       , field3 :: S1 -> Opt T1
       )
  -> VariantState
       ( field1 :: S1
       , field2 :: S1
       , field3 :: S1
       )
  -> Opt
       ( Variant
           ( field1 :: T1
           , field2 :: T1
           , field3 :: T1
           )
       )
testExtractVariant = extractVariant