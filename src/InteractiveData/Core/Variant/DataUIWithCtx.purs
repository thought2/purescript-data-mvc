module InteractiveData.Core.Variant.DataUIWithCtx where

import Prelude

import Data.Identity (Identity)
import Data.Newtype as NT
import Data.Variant (Variant)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import InteractiveData.Core.Types (DataUiItf, DataUICtx, DataUIWithCtx(..))
import InteractiveData.Core.Variant.DataUI (class DataUIVariant, DataUIVariantProps, dataUiVariant)
import InteractiveData.TestTypes (HTML, M1, S1, T1)
import MVC.Variant.Types (VariantMsg, VariantState)
import Type.Proxy (Proxy)

class DataUIWithCtxVariant :: forall k. Row Type -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> k -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class
  DataUIWithCtxVariant datauiwithctxs fm fs srf initsym rcase rmsg rsta r
  -- | datauiwithctxs srf initsym -> rcase rmsg rsta r fm fs
  where
  dataUIWithCtxVariant
    :: Record datauiwithctxs
    -> Proxy initsym
    -> DataUIVariantProps srf initsym
    -> DataUIWithCtx srf fm fs (VariantMsg rcase rmsg) (VariantState rsta) (Variant r)

instance
  ( ApplyCtx (DataUICtx srf fm fs) datauiwithctxs uis
  , DataUIVariant uis srf initsym rcase rmsg rsta r
  ) =>
  DataUIWithCtxVariant datauiwithctxs fm fs srf initsym rcase rmsg rsta r
  where
  dataUIWithCtxVariant datauiwithctxs prxInitSym props =
    DataUIWithCtx \ctx ->
      let
        uis :: Record uis
        uis = mapApplyCtx ctx datauiwithctxs
      in
        dataUiVariant uis prxInitSym props

testDataUIWithCtxVariant
  :: Record
       ( case1 :: DataUIWithCtx HTML Identity Identity M1 S1 T1
       , case2 :: DataUIWithCtx HTML Identity Identity M1 S1 T1
       , case3 :: DataUIWithCtx HTML Identity Identity M1 S1 T1
       )
  -> Proxy "case1"
  -> _
  -> DataUIWithCtx HTML Identity Identity
       ( VariantMsg
           ( case1 :: Unit
           , case2 :: Unit
           , case3 :: Unit
           )
           ( case1 :: M1
           , case2 :: M1
           , case3 :: M1
           )
       )
       ( VariantState
           ( case1 :: S1
           , case2 :: S1
           , case3 :: S1
           )
       )
       ( Variant
           ( case1 :: T1
           , case2 :: T1
           , case3 :: T1
           )
       )
testDataUIWithCtxVariant = dataUIWithCtxVariant

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

class ApplyCtx c datauiwithctxs uis | c datauiwithctxs -> uis where
  mapApplyCtx :: c -> { | datauiwithctxs } -> { | uis }

instance
  ( HMap (FnApplyCtx (DataUICtx srf fm fs)) { | datauiwithctxs } { | uis }
  ) =>
  ApplyCtx (DataUICtx srf fm fs) datauiwithctxs uis where
  mapApplyCtx x = hmap (FnApplyCtx x)

data FnApplyCtx c = FnApplyCtx c

instance
  Mapping
    (FnApplyCtx (DataUICtx srf fm fs))
    (DataUIWithCtx srf fm fs msg sta a)
    (DataUiItf srf msg sta a)
  where
  mapping (FnApplyCtx x) datauiwithctx = NT.unwrap datauiwithctx x
