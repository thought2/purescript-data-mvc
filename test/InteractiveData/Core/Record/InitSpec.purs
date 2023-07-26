module Test.InteractiveData.Core.Record.InitSpec
  ( spec
  ) where

import Prelude

import Data.Maybe (Maybe)
import InteractiveData.Core.Record.Init as ME
import MVC.Record (RecordState)
import Test.InteractiveData.TestTypes (S1, S2, S3, T1, T2, T3)
import Test.Spec (Spec, describe, it)
import Type.Function (type ($))

testInitRecord
  :: Record
       ( field1 :: Maybe T1 -> S1
       , field2 :: Maybe T2 -> S2
       , field3 :: Maybe T3 -> S3
       )
  -> Maybe $ Record
       ( field1 :: T1
       , field2 :: T2
       , field3 :: T3
       )
  -> RecordState
       ( field1 :: S1
       , field2 :: S2
       , field3 :: S3
       )
testInitRecord = ME.initRecord

spec :: Spec Unit
spec = do
  describe "InteractiveData.Core.Record.Init" do
    it "should compile" do
      void $ pure testInitRecord
