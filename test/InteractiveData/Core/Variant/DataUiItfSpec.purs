module Test.InteractiveData.Core.Variant.DataUiItfSpec
  ( spec
  ) where

import Prelude

import Data.Variant (Variant)
import InteractiveData.Core.Types (DataUiItf)
import InteractiveData.Core.Variant.DataUiItf as ME
import Test.InteractiveData.TestTypes (HTML)
import MVC.Variant (CaseKey, VariantMsg, VariantState)
import Test.InteractiveData.TestTypes (M1, M2, M3, S1, S2, S3, T1, T2, T3)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy)

testDataUiItfVariant
  :: Record
       ( case1 :: DataUiItf HTML M1 S1 T1
       , case2 :: DataUiItf HTML M2 S2 T2
       , case3 :: DataUiItf HTML M3 S3 T3
       )
  -> Proxy "case1"
  -> { view ::
         forall msg
          . { caseKey :: CaseKey
            , caseKeys :: Array CaseKey
            , mkMsg :: CaseKey -> msg
            , viewCase :: HTML msg
            }
         -> HTML msg
     }
  -> DataUiItf HTML
       ( VariantMsg
           ( case1 :: Unit
           , case2 :: Unit
           , case3 :: Unit
           )
           ( case1 :: M1
           , case2 :: M2
           , case3 :: M3
           )
       )
       ( VariantState
           ( case1 :: S1
           , case2 :: S2
           , case3 :: S3
           )
       )
       ( Variant
           ( case1 :: T1
           , case2 :: T2
           , case3 :: T3
           )
       )
testDataUiItfVariant = ME.dataUiItfVariant

spec :: Spec Unit
spec = do
  describe "InteractiveData.Core.Variant.DataUiItf" do
    it "should compile" do
      void $ pure testDataUiItfVariant
