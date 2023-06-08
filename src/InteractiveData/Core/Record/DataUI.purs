module InteractiveData.Core.Record.DataUI where

import Data.Identity (Identity)
import Data.Newtype as NT
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import InteractiveData.Core.Record.DataUiItf (class DataUiItfRecord, dataUiItfRecord)
import InteractiveData.Core.Types (DataUiItf, DataUICtx, DataUI(..))
import InteractiveData.TestTypes (HTML, M1, S1, T1)
import MVC.Record (RecordMsg, RecordState)
import MVC.Record.UI (UIRecordProps)

class
  DataUiRecord uis fm fs srf rmsg rsta r
  | uis -> fm fs srf rmsg rsta r
  where
  dataUiRecord
    :: Record uis
    -> UIRecordProps srf (RecordMsg rmsg) (RecordState rsta)
    -> DataUI srf fm fs (RecordMsg rmsg) (RecordState rsta) (Record r)

instance
  ( ApplyCtx (DataUICtx srf fm fs) datauis uis
  , DataUiItfRecord uis srf rmsg rsta r
  ) =>
  DataUiRecord datauis fm fs srf rmsg rsta r
  where
  dataUiRecord datauis props = DataUI \ctx ->
    let
      uis :: Record uis
      uis = mapApplyCtx ctx datauis
    in
      dataUiItfRecord uis props

testDataUiRecord
  :: Record
       ( field1 :: DataUI HTML Identity Identity M1 S1 T1
       )
  -> { viewEntries ::
         Array
           { key :: String
           , viewValue ::
               HTML
                 ( RecordMsg
                     ( field1 :: M1
                     )
                 )
           }
         -> HTML
              ( RecordMsg
                  ( field1 :: M1
                  )
              )
     }
  -> DataUI HTML Identity Identity
       ( RecordMsg
           ( field1 :: M1
           )
       )
       ( RecordState
           ( field1 :: S1
           )
       )
       ( Record
           ( field1 :: T1
           )
       )
testDataUiRecord = dataUiRecord

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

class ApplyCtx a datauis uis where
  mapApplyCtx :: a -> { | datauis } -> { | uis }

instance (HMap (FnApplyCtx a) { | datauis } { | uis }) => ApplyCtx a datauis uis where
  mapApplyCtx x = hmap (FnApplyCtx x)

data FnApplyCtx a = FnApplyCtx a

instance Mapping (FnApplyCtx (DataUICtx srf fm fs)) (DataUI srf fm fs msg sta a) (DataUiItf srf msg sta a) where
  mapping (FnApplyCtx x) dataUi = NT.unwrap dataUi x
