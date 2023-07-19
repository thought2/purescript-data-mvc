module InteractiveData.Core.Variant.DataUiItf where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.Newtype as NT
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import InteractiveData.Core.Types (DataUiItf(..), IDError(..), Opt)
import InteractiveData.Core.Variant.Extract (class ExtractVariant, extractVariant)
import InteractiveData.Core.Variant.Init (class InitVariant, initVariant)
import InteractiveData.TestTypes (HTML, M1, M2, M3, S1, S2, S3, T1, T2, T3)
import MVC.Variant.Types (VariantMsg, VariantState)
import MVC.Variant.Update (class UpdateVariant, updateVariant)
import MVC.Variant.View (class ViewVariant, ViewArgs, viewVariant)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

type DataUiItfVariantProps :: forall k. (Type -> Type) -> k -> Type
type DataUiItfVariantProps srf initsym =
  { view :: forall msg. ViewArgs srf msg -> srf msg
  }

class DataUiItfVariant :: Row Type -> (Type -> Type) -> Symbol -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class
  DataUiItfVariant uis srf initsym rcase rmsg rsta r
  | uis srf initsym rcase -> rmsg rsta r
  where
  dataUiItfVariant
    :: Record uis
    -> Proxy initsym
    -> DataUiItfVariantProps srf initsym
    -> DataUiItf srf (VariantMsg rcase rmsg) (VariantState rsta) (Variant r)

instance
  ( MapProp "extract" uis extracts
  , MapProp "init" uis inits
  , MapProp "update" uis updates
  , MapProp "view" uis views

  , InitVariant inits initsym r rsta
  , UpdateVariant inits' updates rcase rmsg rsta
  , ViewVariant srf views rcase rmsg rsta
  , ExtractVariant extracts rsta r

  , MapInits inits inits'
  ) =>
  DataUiItfVariant uis srf initsym rcase rmsg rsta r
  where
  dataUiItfVariant uis prxInitSym props =
    DataUiItf
      { init, update, view, extract, name }

    where
    init = initVariant inits prxInitSym
    update = updateVariant inits' updates
    view = viewVariant { view: props.view } views
    extract = extractVariant extracts
    name = "Variant"

    inits' = mapInits inits

    inits = mapProp prxInit uis
    updates = mapProp prxUpdate uis
    views = mapProp prxView uis
    extracts = mapProp prxExtract uis

    prxInit = Proxy :: _ "init"
    prxUpdate = Proxy :: _ "update"
    prxView = Proxy :: _ "view"
    prxExtract = Proxy :: _ "extract"

---

testDataUiItfVariant
  :: Record
       ( case1 :: DataUiItf HTML M1 S1 T1
       , case2 :: DataUiItf HTML M2 S2 T2
       , case3 :: DataUiItf HTML M3 S3 T3
       )
  -> Proxy "case1"
  -> _
  -> DataUiItf HTML
       ( VariantMsg
           ( case1 :: Unit
           , case2 :: Unit
           , case3 :: Unit
           )
           ( case1 :: M1
           , case2 :: M2
           , case3 :: M3
           )
       )
       ( VariantState
           ( case1 :: S1
           , case2 :: S2
           , case3 :: S3
           )
       )
       ( Variant
           ( case1 :: T1
           , case2 :: T2
           , case3 :: T3
           )
       )
testDataUiItfVariant = dataUiItfVariant

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

---

class MapInits inits inits' | inits -> inits' where
  mapInits :: Record inits -> Record inits'

instance (HMap FnConvertInit { | inits } { | inits' }) => MapInits inits inits' where
  mapInits = hmap FnConvertInit

data FnConvertInit = FnConvertInit

instance Mapping FnConvertInit (Opt a -> b) b where
  mapping _ f = f $ Left IDErrNotYetDefined