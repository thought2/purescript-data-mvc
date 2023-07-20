module InteractiveData.Core.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as NT
import Data.Profunctor (lcmap)
import Data.Show.Generic (genericShow)
import MVC.Types as MVC

type Extract sta a = sta -> Opt a

type Update msg sta = msg -> sta -> sta

type Init sta a = Maybe a -> sta

type View :: forall k. (k -> Type) -> k -> Type -> Type
type View srf msg sta = sta -> srf msg

---

newtype DataUiItf srf msg sta a =
  DataUiItf
    { init :: Maybe a -> sta
    , update :: msg -> sta -> sta
    , view :: sta -> srf msg
    , extract :: sta -> Opt a
    , name :: String
    }

dataUiItfToUI :: forall html msg sta a. DataUiItf html msg sta a -> MVC.UI html msg sta
dataUiItfToUI (DataUiItf dataUi) =
  { init: dataUi.init Nothing
  , update: dataUi.update
  , view: dataUi.view
  }

---

newtype DataUI srf fm fs msg sta a =
  DataUI
    (DataUICtx srf fm fs -> DataUiItf srf msg sta a)

runDataUi
  :: forall srf fm fs msg sta a
   . DataUI srf fm fs msg sta a
  -> DataUICtx srf fm fs
  -> DataUiItf srf msg sta a
runDataUi (DataUI dataUi) ctx = dataUi ctx

runDataUiFinal
  :: forall srf fm fs msg sta a
   . DataUI srf fm fs msg sta a
  -> DataUICtx srf fm fs
  -> DataUiItf srf (fm msg) (fs sta) a
runDataUiFinal dataUi ctx = runDataUi (applyWrap dataUi) ctx

applyWrap
  :: forall srf fm fs msg sta a
   . DataUI srf fm fs msg sta a
  -> DataUI srf fm fs (fm msg) (fs sta) a
applyWrap (DataUI mkDataUi) = DataUI \c@(DataUICtx ctx) ->
  ctx.wrap $ mkDataUi c

applyDataUi
  :: forall html fm fs msg1 msg2 sta1 sta2 a1 a2
   . { extract :: Extract sta1 a1 -> Extract sta2 a2
     , init :: Init sta1 a1 -> Init sta2 a2
     , name :: String
     , update :: Update msg1 sta1 -> Update msg2 sta2
     , view :: View html msg1 sta1 -> View html msg2 sta2
     }
  -> DataUI html fm fs msg1 sta1 a1
  -> DataUI html fm fs msg2 sta2 a2
applyDataUi r ui1 = DataUI \ctx -> DataUiItf
  { name: r.name
  , view:
      r.view (NT.unwrap $ runDataUi ui1 ctx).view
  , extract:
      r.extract (NT.unwrap $ runDataUi ui1 ctx).extract
  , update:
      r.update (NT.unwrap $ runDataUi ui1 ctx).update
  , init:
      r.init (NT.unwrap $ runDataUi ui1 ctx).init
  }

type RefineOpts a b =
  { typeName :: String
  , refine :: a -> Opt b
  , unrefine :: b -> a
  }

refineDataUi :: forall srf fm fs msg sta a b. RefineOpts a b -> DataUI srf fm fs msg sta a -> DataUI srf fm fs msg sta b
refineDataUi { typeName, refine, unrefine } (DataUI mkDataUi) = DataUI \ctx ->
  let
    DataUiItf dataUi = mkDataUi ctx
  in
    DataUiItf
      { extract: dataUi.extract >=> refine
      , init: lcmap (map unrefine) dataUi.init
      , name: typeName
      , update: dataUi.update
      , view: dataUi.view
      }

---

newtype DataUICtx html fm fs = DataUICtx (DataUICtxImpl html fm fs)

type DataUICtxImpl html fm fs =
  { wrap ::
      forall msg sta a
       . DataUiItf html msg sta a
      -> DataUiItf html (fm msg) (fs sta) a
  }

unDataUICtx :: forall html fm fs. DataUICtx html fm fs -> DataUICtxImpl html fm fs
unDataUICtx (DataUICtx impl) = impl

---

data DataPathSegment
  = SegCase String
  | SegField DataPathSegmentField

data DataPathSegmentField
  = SegStaticKey String
  | SegStaticIndex Int
  | SegDynamicKey String
  | SegDynamicIndex Int
  | SegVirtualKey String

type DataPath = Array DataPathSegment

type Opt a = Either (NonEmptyArray IDError) a

data IDErrorCase = IDErrNotYetDefined | IDErrMsg String

scopeError :: DataPathSegment -> IDError -> IDError
scopeError seg (IDError path case_) = IDError ([ seg ] <> path) case_

scopeErrors :: DataPathSegment -> NonEmptyArray IDError -> NonEmptyArray IDError
scopeErrors seg = map (scopeError seg)

scopeOpt :: forall a. DataPathSegment -> Opt a -> Opt a
scopeOpt seg = lmap (scopeErrors seg)

data IDError = IDError DataPath IDErrorCase

derive instance Ord IDError

derive instance Eq IDError

derive instance Generic IDError _

derive instance Generic DataPathSegment _
derive instance Generic DataPathSegmentField _
derive instance Generic IDErrorCase _

instance Show IDError where
  show = genericShow

derive instance Newtype (DataUI srf fm fs msg sta a) _

derive instance Newtype (DataUiItf srf msg sta a) _

instance Show DataPathSegment where
  show = genericShow

instance Show DataPathSegmentField where
  show = genericShow

instance Show IDErrorCase where
  show = genericShow

derive instance Ord DataPathSegment
derive instance Ord DataPathSegmentField
derive instance Ord IDErrorCase

derive instance Eq DataPathSegment
derive instance Eq DataPathSegmentField
derive instance Eq IDErrorCase