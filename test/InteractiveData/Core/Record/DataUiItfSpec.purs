module Test.InteractiveData.Core.Record.DataUiItfSpec
  ( spec
  ) where

import Prelude

import InteractiveData.Core.Record.DataUiItf as ME
import InteractiveData.Core.Types (DataUiItf)
import MVC.Record (RecordMsg, RecordState)
import Test.InteractiveData.TestTypes (HTML, M1, M2, M3, S1, S2, S3, T1, T2, T3)
import Test.Spec (Spec, describe, it)

testDataUiItfRecord
  :: { viewEntries ::
         Array
           { key :: String
           , viewValue ::
               HTML
                 ( RecordMsg
                     ( field1 :: M1
                     , field2 :: M2
                     , field3 :: M3
                     )
                 )
           }
         -> HTML
              ( RecordMsg
                  ( field1 :: M1
                  , field2 :: M2
                  , field3 :: M3
                  )
              )
     }
  -> Record
       ( field1 :: DataUiItf HTML M1 S1 T1
       , field2 :: DataUiItf HTML M2 S2 T2
       , field3 :: DataUiItf HTML M3 S3 T3
       )
  -> DataUiItf HTML
       ( RecordMsg
           ( field1 :: M1
           , field2 :: M2
           , field3 :: M3
           )
       )
       ( RecordState
           ( field1 :: S1
           , field2 :: S2
           , field3 :: S3
           )
       )
       ( Record
           ( field1 :: T1
           , field2 :: T2
           , field3 :: T3
           )
       )
testDataUiItfRecord = ME.dataUiItfRecord

spec :: Spec Unit
spec = do
  describe "InteractiveData.Core.Record.DataUiItf" do
    it "should compile" do
      void $ pure testDataUiItfRecord
