module InteractiveData.Core.Variant.UICtx where

import Prelude

import Data.Identity (Identity)
import Data.Newtype as NT
import Data.Variant (Variant)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import InteractiveData.Core.Types (Ctx, UI, UICtx(..))
import InteractiveData.Core.Variant.UI (class UIVariant, UIVariantProps, uiVariant)
import InteractiveData.TestTypes (HTML, M1, S1, T1)
import MVC.Variant.Types (VariantMsg, VariantState)
import Type.Proxy (Proxy)

class UICtxVariant :: forall k. Row Type -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> k -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class
  UICtxVariant uictxs fm fs srf initsym rcase rmsg rsta r
  -- | uictxs srf initsym -> rcase rmsg rsta r fm fs
  where
  uiCtxVariant
    :: Record uictxs
    -> Proxy initsym
    -> UIVariantProps srf initsym
    -> UICtx srf fm fs (VariantMsg rcase rmsg) (VariantState rsta) (Variant r)

instance
  ( ApplyCtx (Ctx srf fm fs) uictxs uis
  , UIVariant uis srf initsym rcase rmsg rsta r
  ) =>
  UICtxVariant uictxs fm fs srf initsym rcase rmsg rsta r
  where
  uiCtxVariant uictxs prxInitSym props =
    UICtx \ctx ->
      let
        uis :: Record uis
        uis = mapApplyCtx ctx uictxs
      in
        uiVariant uis prxInitSym props

testUiCtxVariant
  :: Record
       ( case1 :: UICtx HTML Identity Identity M1 S1 T1
       , case2 :: UICtx HTML Identity Identity M1 S1 T1
       , case3 :: UICtx HTML Identity Identity M1 S1 T1
       )
  -> Proxy "case1"
  -> _
  -> UICtx HTML Identity Identity
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
testUiCtxVariant = uiCtxVariant

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

class ApplyCtx c uictxs uis | c uictxs -> uis where
  mapApplyCtx :: c -> { | uictxs } -> { | uis }

instance
  ( HMap (FnApplyCtx (Ctx srf fm fs)) { | uictxs } { | uis }
  ) =>
  ApplyCtx (Ctx srf fm fs) uictxs uis where
  mapApplyCtx x = hmap (FnApplyCtx x)

data FnApplyCtx c = FnApplyCtx c

instance
  Mapping
    (FnApplyCtx (Ctx srf fm fs))
    (UICtx srf fm fs msg sta a)
    (UI srf msg sta a)
  where
  mapping (FnApplyCtx x) uictx = NT.unwrap uictx x
