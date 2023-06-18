module InteractiveData.Core.Variant.DataUI where

import Prelude

import Data.Identity (Identity)
import Data.Variant (Variant)
import InteractiveData.Core.ApplyCtx (class ApplyCtx, mapApplyCtx)
import InteractiveData.Core.Types (DataUI(..), DataUICtx)
import InteractiveData.Core.Variant.DataUiItf (class DataUiItfVariant, DataUiItfVariantProps, dataUiItfVariant)
import InteractiveData.TestTypes (HTML, M1, S1, T1)
import MVC.Variant.Types (CaseKey, VariantMsg, VariantState)
import Type.Proxy (Proxy)

class DataUiVariant :: forall k. Row Type -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> k -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class
  DataUiVariant datauis fm fs srf initsym rcase rmsg rsta r
  -- | datauis srf initsym -> rcase rmsg rsta r fm fs
  where
  dataUiVariant
    :: Record datauis
    -> Proxy initsym
    -> DataUiItfVariantProps srf initsym
    -> DataUI srf fm fs (VariantMsg rcase rmsg) (VariantState rsta) (Variant r)

instance
  ( ApplyCtx (DataUICtx srf fm fs) datauis uis
  , DataUiItfVariant uis srf initsym rcase rmsg rsta r
  ) =>
  DataUiVariant datauis fm fs srf initsym rcase rmsg rsta r
  where
  dataUiVariant datauis prxInitSym props =
    DataUI \ctx ->
      let
        uis :: Record uis
        uis = mapApplyCtx ctx datauis
      in
        dataUiItfVariant uis prxInitSym props

testDataUiVariant
  :: Record
       ( case1 :: DataUI HTML Identity Identity M1 S1 T1
       , case2 :: DataUI HTML Identity Identity M1 S1 T1
       , case3 :: DataUI HTML Identity Identity M1 S1 T1
       )
  -> Proxy "case1"
  -> { view :: forall msg.
            { caseKey :: CaseKey
            , caseKeys :: Array CaseKey
            , mkMsg :: CaseKey -> msg
            , viewCase :: HTML msg
            }
            -> HTML msg
}

  -> DataUI HTML Identity Identity
       ( VariantMsg
           ( case1 :: Unit
           , case2 :: Unit
           , case3 :: Unit
           )
           ( case1 :: Identity M1
           , case2 :: Identity M1
           , case3 :: Identity M1
           )
       )
       ( VariantState
           ( case1 :: Identity S1
           , case2 :: Identity S1
           , case3 :: Identity S1
           )
       )
       ( Variant
           ( case1 :: T1
           , case2 :: T1
           , case3 :: T1
           )
       )
testDataUiVariant = dataUiVariant
