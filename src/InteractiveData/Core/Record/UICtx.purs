module InteractiveData.Core.Record.UICtx where

import Data.Identity (Identity)
import Data.Newtype as NT
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import InteractiveData.Core.Record.UI (class UIRecord, uiRecord)
import InteractiveData.Core.Types (Ctx, UI, UICtx(..))
import InteractiveData.TestTypes (HTML, M1, S1, T1)
import MVC.Record (RecordMsg, RecordState)
import MVC.Record.UI (UIRecordProps)

class
  UICtxRecord uis fm fs srf rmsg rsta r
  | uis -> fm fs srf rmsg rsta r
  where
  uiCtxRecord
    :: Record uis
    -> UIRecordProps srf (RecordMsg rmsg) (RecordState rsta)
    -> UICtx srf fm fs (RecordMsg rmsg) (RecordState rsta) (Record r)

instance
  ( ApplyCtx (Ctx srf fm fs) uictxs uis
  , UIRecord uis srf rmsg rsta r
  ) =>
  UICtxRecord uictxs fm fs srf rmsg rsta r
  where
  uiCtxRecord uictxs props = UICtx \ctx ->
    let
      uis :: Record uis
      uis = mapApplyCtx ctx uictxs
    in
      uiRecord uis props

testUiCtxRecord
  :: Record
       ( field1 :: UICtx HTML Identity Identity M1 S1 T1
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
  -> UICtx HTML Identity Identity
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
testUiCtxRecord = uiCtxRecord

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

class ApplyCtx a uictxs uis where
  mapApplyCtx :: a -> { | uictxs } -> { | uis }

instance (HMap (FnApplyCtx a) { | uictxs } { | uis }) => ApplyCtx a uictxs uis where
  mapApplyCtx x = hmap (FnApplyCtx x)

data FnApplyCtx a = FnApplyCtx a

instance Mapping (FnApplyCtx (Ctx srf fm fs)) (UICtx srf fm fs msg sta a) (UI srf msg sta a) where
  mapping (FnApplyCtx x) uictx = NT.unwrap uictx x
