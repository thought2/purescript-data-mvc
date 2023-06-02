module InteractiveData.Core.Record.UI where


import Prelude

import Data.Identity (Identity)
import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import InteractiveData.Core.Record.Extract (class ExtractRecord, extractRecord)
import InteractiveData.Core.Record.Init (class InitRecord, initRecord)
import InteractiveData.Core.Types (Ctx, UI, UICtx(..))
import InteractiveData.TestTypes (HTML, M1(..), M2(..), M3(..), S1(..), S2(..), S3(..), T1(..), T2(..), T3(..))
import MVC.Record (class UpdateRecord, class ViewRecord, RecordMsg, RecordState, updateRecord, viewRecord)
import MVC.Record.UI (UIRecordProps)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class UIRecord uis srf rmsg rsta r where
  uiRecord
    :: Record uis
    -> UIRecordProps srf (RecordMsg rmsg) (RecordState rsta)
    -> UI srf (RecordMsg rmsg) (RecordState rsta) (Record r)

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
  UIRecord uis srf rmsg rsta r
  where
  uiRecord uis props = { init, update, view, extract, name }

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

testUIRecord
  :: Record
       ( field1 :: UI HTML M1 S1 T1
       , field2 :: UI HTML M2 S2 T2
       , field3 :: UI HTML M3 S3 T3
       )
  -> _
  -> UI HTML
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
testUIRecord = uiRecord

---

-- class
--   UICtxRecord uis fs fm srf rmsg rsta r
--   | uis -> fs fm srf rmsg rsta r
--   where
--   uiCtxRecord
--     :: Record uis
--     -> UIRecordProps srf (RecordMsg rmsg) (RecordState rsta)
--     -> UICtx srf fs fm (RecordMsg rmsg) (RecordState rsta) (Record r)

-- instance
--   ( HMap
--       (FnUnwrapNT |>>> FnApplyFlipped (Ctx srf fm fs))
--       (Record uictxs)
--       (Record uis)
--   , UIRecord uis srf rmsg rsta r
--   ) =>
--   UICtxRecord uictxs fm fs srf rmsg rsta r
--   where
--   uiCtxRecord uictxs props = UICtx \ctx ->
--     let
--       uis :: Record uis
--       uis = hmap (FnUnwrapNT |>>> (FnApplyFlipped ctx)) uictxs
--     in
--       uiRecord uis props

-- -- data F = F

-- -- instance Mapping F (UICtx srf fm fs msg sta a) (UI srf msg sta a) where
-- --   mapping _ uictx = unwrap uictx

-- t1
--   :: Record
--        ( field1 :: UICtx HTML Identity Identity M1 S1 T1
--        )
--   -> _
--   -> UICtx HTML Identity Identity
--        ( RecordMsg
--            ( field1 :: M1
--            )
--        )
--        ( RecordState
--            ( field1 :: S1
--            )
--        )
--        ( Record
--            ( field1 :: T1
--            )
--        )
-- t1 = uiCtxRecord

---

class MapProp :: Symbol -> Row Type -> Row Type -> Constraint
class MapProp sym ri ro | sym ri -> ro where
  mapProp :: Proxy sym -> { | ri } -> { | ro }

instance
  ( HMap (FnRecordGet sym) (Record ri) (Record ro)
  ) =>
  MapProp sym ri ro
  where
  mapProp sym = hmap (FnRecordGet sym)

data FnRecordGet sym = FnRecordGet (Proxy sym)

instance
  ( Row.Cons sym a rx r
  , IsSymbol sym
  ) =>
  Mapping (FnRecordGet sym) (Record r) a
  where
  mapping (FnRecordGet prxSym) = Record.get prxSym