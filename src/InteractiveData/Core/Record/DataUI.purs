module InteractiveData.Core.Record.DataUI where

import Data.Identity (Identity)
import Data.Symbol (class IsSymbol)
import InteractiveData.Core.Record.DataUiItf (class DataUiItfRecord, dataUiItfRecord)
import InteractiveData.Core.Types (DataUI(..), DataUICtx, DataUiItf, applyWrap, runDataUi)
import InteractiveData.TestTypes (HTML, M1, M2, M3, S1, S2, S3, T1, T2, T3)
import MVC.Record (RecordMsg, RecordState)
import MVC.Record.UI (UIRecordProps)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

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

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

class ApplyCtx a datauis uis | datauis -> uis a where
  mapApplyCtx :: a -> { | datauis } -> { | uis }

instance (ApplyCtxRL rl a datauis uis, RowToList datauis rl) => ApplyCtx a datauis uis where
  mapApplyCtx = mapApplyCtxRL prxRl
    where
    prxRl = Proxy :: _ rl

---

class ApplyCtxRL :: RowList Type -> Type -> Row Type -> Row Type -> Constraint
class ApplyCtxRL rl a datauis uis | datauis -> uis a where
  mapApplyCtxRL :: Proxy rl -> a -> { | datauis } -> { | uis }

instance ApplyCtxRL RL.Nil a datauis () where
  mapApplyCtxRL _ _ _ = {}

instance
  ( ApplyCtxRL rl' (DataUICtx srf fm fs) datauis uis'
  , Row.Cons sym (DataUiItf srf (fm msg) (fs sta) a) uis' uis
  , Row.Cons sym (DataUI srf fm fs msg sta a) datauisx datauis
  , IsSymbol sym
  , Row.Lacks sym uis'
  ) =>
  ApplyCtxRL (RL.Cons sym x rl') (DataUICtx srf fm fs) datauis uis where
  mapApplyCtxRL _ ctx datauis =
    Record.insert prxSym ui tail
    where
    dataui :: DataUI srf fm fs msg sta a
    dataui = Record.get prxSym datauis

    dataui' :: DataUI srf fm fs (fm msg) (fs sta) a
    dataui' = applyWrap dataui

    ui :: DataUiItf srf (fm msg) (fs sta) a
    ui = runDataUi dataui' ctx

    tail :: { | uis' }
    tail = mapApplyCtxRL (Proxy :: Proxy rl') ctx datauis

    prxSym = Proxy :: _ sym

