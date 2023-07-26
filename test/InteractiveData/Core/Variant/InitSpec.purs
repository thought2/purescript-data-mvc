module Test.InteractiveData.Core.Variant.InitSpec
  ( spec
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import InteractiveData.Core.Variant.Init as ME
import MVC.Variant (VariantState)
import Test.InteractiveData.TestTypes (S1, S2, S3, T1, T2, T3)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy)

testInitVariant
  :: Record
       ( case1 :: Maybe T1 -> S1
       , case2 :: Maybe T2 -> S2
       , case3 :: Maybe T3 -> S3
       )
  -> Proxy "case2"
  -> Maybe
       ( Variant
           ( case1 :: T1
           , case2 :: T2
           , case3 :: T3
           )
       )
  -> VariantState
       ( case1 :: S1
       , case2 :: S2
       , case3 :: S3
       )
testInitVariant = ME.initVariant

spec :: Spec Unit
spec = do
  describe "InteractiveData.Core.Variant.Init" do
    it "should compile" do
      void $ pure testInitVariant
