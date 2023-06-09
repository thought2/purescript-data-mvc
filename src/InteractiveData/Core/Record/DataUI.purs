module InteractiveData.Core.Record.DataUI where

import Prelude

import Data.Identity (Identity)
import Data.Newtype as NT
import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import InteractiveData.Core.Record.DataUiItf (class DataUiItfRecord, dataUiItfRecord)
import InteractiveData.Core.Types (DataUI(..), DataUICtx, DataUiItf, runDataUi)
import InteractiveData.TestTypes (HTML, M1, M2, M3, S1, S2, S3, T1, T2, T3)
import MVC.Record (RecordMsg, RecordState)
import MVC.Record.UI (UIRecordProps)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class
  DataUiRecord datauis fm fs srf rmsg rsta r
    | datauis -> fm fs srf rmsg rsta r
  where
  dataUiRecord
    :: Record datauis
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
       , field2 :: DataUI HTML Identity Identity M2 S2 T2
       , field3 :: DataUI HTML Identity Identity M3 S3 T3
       )
  -> { viewEntries ::
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
  -> DataUI HTML Identity Identity
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
testDataUiRecord = dataUiRecord

testDataUiRecord2
  :: Record
       ( field1 :: DataUI HTML Identity Identity M1 S1 T1
       )
  -> _
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
  , Row.Cons sym (DataUiItf srf msg sta a) uis' uis
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

    ui :: DataUiItf srf msg sta a
    ui = runDataUi dataui ctx

    tail :: { | uis' }
    tail = mapApplyCtxRL (Proxy :: Proxy rl') ctx datauis

    prxSym = Proxy :: _ sym

