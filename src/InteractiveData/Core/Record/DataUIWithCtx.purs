module InteractiveData.Core.Record.DataUIWithCtx where

import Data.Identity (Identity)
import Data.Newtype as NT
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import InteractiveData.Core.Record.DataUI (class DataUIRecord, dataUiRecord)
import InteractiveData.Core.Types (DataUiItf, DataUICtx, DataUIWithCtx(..))
import InteractiveData.TestTypes (HTML, M1, S1, T1)
import MVC.Record (RecordMsg, RecordState)
import MVC.Record.UI (UIRecordProps)

class
  DataUIWithCtxRecord uis fm fs srf rmsg rsta r
  | uis -> fm fs srf rmsg rsta r
  where
  dataUIWithCtxRecord
    :: Record uis
    -> UIRecordProps srf (RecordMsg rmsg) (RecordState rsta)
    -> DataUIWithCtx srf fm fs (RecordMsg rmsg) (RecordState rsta) (Record r)

instance
  ( ApplyCtx (DataUICtx srf fm fs) datauiwithctxs uis
  , DataUIRecord uis srf rmsg rsta r
  ) =>
  DataUIWithCtxRecord datauiwithctxs fm fs srf rmsg rsta r
  where
  dataUIWithCtxRecord datauiwithctxs props = DataUIWithCtx \ctx ->
    let
      uis :: Record uis
      uis = mapApplyCtx ctx datauiwithctxs
    in
      dataUiRecord uis props

testDataUIWithCtxRecord
  :: Record
       ( field1 :: DataUIWithCtx HTML Identity Identity M1 S1 T1
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
  -> DataUIWithCtx HTML Identity Identity
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
testDataUIWithCtxRecord = dataUIWithCtxRecord

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

class ApplyCtx a datauiwithctxs uis where
  mapApplyCtx :: a -> { | datauiwithctxs } -> { | uis }

instance (HMap (FnApplyCtx a) { | datauiwithctxs } { | uis }) => ApplyCtx a datauiwithctxs uis where
  mapApplyCtx x = hmap (FnApplyCtx x)

data FnApplyCtx a = FnApplyCtx a

instance Mapping (FnApplyCtx (DataUICtx srf fm fs)) (DataUIWithCtx srf fm fs msg sta a) (DataUiItf srf msg sta a) where
  mapping (FnApplyCtx x) datauiwithctx = NT.unwrap datauiwithctx x
