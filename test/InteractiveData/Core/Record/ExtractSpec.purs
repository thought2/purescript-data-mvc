module Test.InteractiveData.Core.Record.ExtractSpec
  ( spec
  ) where

import Prelude

import InteractiveData.Core.Record.Extract as ME
import InteractiveData.Core.Types (DataResult)
import MVC.Record (RecordState)
import Test.InteractiveData.TestTypes (S1, S2, S3, T1, T2, T3)
import Test.Spec (Spec, describe, it)
import Type.Function (type ($))

testExtractRecord
  :: Record
       ( field1 :: S1 -> DataResult T1
       , field2 :: S2 -> DataResult T2
       , field3 :: S3 -> DataResult T3
       )
  -> RecordState
       ( field1 :: S1
       , field2 :: S2
       , field3 :: S3
       )
  -> DataResult $ Record
       ( field1 :: T1
       , field2 :: T2
       , field3 :: T3
       )
testExtractRecord = ME.extractRecord

spec :: Spec Unit
spec = do
  describe "InteractiveData.Core.Record.Extract" do
    it "should compile" do
      void $ pure testExtractRecord
