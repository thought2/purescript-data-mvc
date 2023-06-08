module InteractiveData.Core.Record.DataUiItf where

import Prelude

import Data.Newtype (class Newtype)
import Data.Newtype as NT
import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import InteractiveData.Core.Record.Extract (class ExtractRecord, extractRecord)
import InteractiveData.Core.Record.Init (class InitRecord, initRecord)
import InteractiveData.Core.Types (DataUiItf(..))
import InteractiveData.TestTypes (HTML, M1, M2, M3, S1, S2, S3, T1, T2, T3)
import MVC.Record (class UpdateRecord, class ViewRecord, RecordMsg, RecordState, updateRecord, viewRecord)
import MVC.Record.UI (UIRecordProps)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

class DataUiItfRecord uis srf rmsg rsta r where
  dataUiItfRecord
    :: Record uis
    -> UIRecordProps srf (RecordMsg rmsg) (RecordState rsta)
    -> DataUiItf srf (RecordMsg rmsg) (RecordState rsta) (Record r)

instance
  ( MapProp "extract" uis extracts
  , MapProp "init" uis inits
  , MapProp "update" uis updates
  , MapProp "view" uis views

  , InitRecord inits r rsta
  , UpdateRecord updates rmsg rsta
  , ViewRecord srf views rmsg rsta
  , ExtractRecord extracts rsta r
  ) =>
  DataUiItfRecord uis srf rmsg rsta r
  where
  dataUiItfRecord uis props = DataUiItf { init, update, view, extract, name }

    where
    init = initRecord inits
    update = updateRecord updates
    view = viewRecord views { viewEntries: props.viewEntries }
    extract = extractRecord extracts
    name = "Record"

    inits = mapProp prxInit uis
    updates = mapProp prxUpdate uis
    views = mapProp prxView uis
    extracts = mapProp prxExtract uis

    prxInit = Proxy :: _ "init"
    prxUpdate = Proxy :: _ "update"
    prxView = Proxy :: _ "view"
    prxExtract = Proxy :: _ "extract"

---

testDataUiItfRecord
  :: Record
       ( field1 :: DataUiItf HTML M1 S1 T1
       , field2 :: DataUiItf HTML M2 S2 T2
       , field3 :: DataUiItf HTML M3 S3 T3
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
testDataUiItfRecord = dataUiItfRecord

---

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

class MapProp :: Symbol -> Row Type -> Row Type -> Constraint
class MapProp sym ri ro | sym ri -> ro where
  mapProp :: Proxy sym -> { | ri } -> { | ro }

instance
  ( HMap (FnRecordGet sym) (Record ri) (Record ro)
  ) =>
  MapProp sym ri ro
  where
  mapProp sym = hmap (FnRecordGet sym)

data FnRecordGet :: forall k. k -> Type
data FnRecordGet sym = FnRecordGet (Proxy sym)

instance
  ( Row.Cons sym a rx r
  , IsSymbol sym
  , Newtype nt (Record r)
  ) =>
  Mapping (FnRecordGet sym) nt a
  where
  mapping (FnRecordGet prxSym) = Record.get prxSym <<< NT.unwrap

