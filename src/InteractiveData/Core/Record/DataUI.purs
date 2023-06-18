module InteractiveData.Core.Record.DataUI where

import Data.Identity (Identity)
import InteractiveData.Core.ApplyCtx (class ApplyCtx, mapApplyCtx)
import InteractiveData.Core.Record.DataUiItf (class DataUiItfRecord, dataUiItfRecord)
import InteractiveData.Core.Types (DataUI(..), DataUICtx)
import InteractiveData.TestTypes (HTML, M1, M2, M3, S1, S2, S3, T1, T2, T3)
import MVC.Record (RecordMsg, RecordState)
import MVC.Record.UI (UIRecordProps)

class
  DataUiRecord datauis fm fs srf rmsg rsta r
  | datauis -> fm fs srf rmsg rsta r
  where
  dataUiRecord
    :: UIRecordProps srf (RecordMsg rmsg) (RecordState rsta)
    -> Record datauis
    -> DataUI srf fm fs (RecordMsg rmsg) (RecordState rsta) (Record r)

instance
  ( ApplyCtx (DataUICtx srf fm fs) datauis uis
  , DataUiItfRecord uis srf rmsg rsta r
  ) =>
  DataUiRecord datauis fm fs srf rmsg rsta r
  where
  dataUiRecord props datauis = DataUI \ctx ->
    let
      uis :: Record uis
      uis = mapApplyCtx ctx datauis
    in
      dataUiItfRecord props uis

testDataUiRecord
  :: { viewEntries ::
         Array
           { key :: String
           , viewValue ::
               HTML
                 ( RecordMsg
                     ( field1 :: Identity M1
                     , field2 :: Identity M2
                     , field3 :: Identity M3
                     )
                 )
           }
         -> HTML
              ( RecordMsg
                  ( field1 :: Identity M1
                  , field2 :: Identity M2
                  , field3 :: Identity M3
                  )
              )
     }
  -> Record
       ( field1 :: DataUI HTML Identity Identity M1 S1 T1
       , field2 :: DataUI HTML Identity Identity M2 S2 T2
       , field3 :: DataUI HTML Identity Identity M3 S3 T3
       )
  -> DataUI HTML Identity Identity
       ( RecordMsg
           ( field1 :: Identity M1
           , field2 :: Identity M2
           , field3 :: Identity M3
           )
       )
       ( RecordState
           ( field1 :: Identity S1
           , field2 :: Identity S2
           , field3 :: Identity S3
           )
       )
       ( Record
           ( field1 :: T1
           , field2 :: T2
           , field3 :: T3
           )
       )
testDataUiRecord = dataUiRecord

testDataUiRecord2
  :: { viewEntries ::
         Array
           { key :: String
           , viewValue ::
               HTML
                 ( RecordMsg
                     ( field1 :: Identity M1
                     )
                 )
           }
         -> HTML
              ( RecordMsg
                  ( field1 :: Identity M1
                  )
              )
     }
  -> Record
       ( field1 :: DataUI HTML Identity Identity M1 S1 T1
       )
  -> DataUI HTML Identity Identity
       ( RecordMsg
           ( field1 :: Identity M1
           )
       )
       ( RecordState
           ( field1 :: Identity S1
           )
       )
       { field1 :: T1
       }

testDataUiRecord2 = dataUiRecord
