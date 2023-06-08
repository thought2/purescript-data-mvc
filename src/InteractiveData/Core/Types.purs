module InteractiveData.Core.Types where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Profunctor (lcmap)
import Data.Show.Generic (genericShow)
import MVC.Types as MVC

---

newtype DataUI srf msg sta a =
  DataUI
    { init :: Opt a -> sta
    , update :: msg -> sta -> sta
    , view :: sta -> srf msg
    , extract :: sta -> Opt a
    , name :: String
    }

dataUItoUI :: forall html msg sta a. DataUI html msg sta a -> MVC.UI html msg sta
dataUItoUI (DataUI dataUi) =
  { init: dataUi.init $ Left ErrNotYetDefined
  , update: dataUi.update
  , view: dataUi.view
  }

---

newtype DataUIWithCtx srf fm fs msg sta a =
  DataUIWithCtx
    (DataUICtx srf fm fs -> DataUI srf msg sta a)

runDataUIWithCtx
  :: forall srf fm fs msg sta a
   . DataUIWithCtx srf fm fs msg sta a
  -> DataUICtx srf fm fs
  -> DataUI srf msg sta a
runDataUIWithCtx (DataUIWithCtx dataUi) ctx = dataUi ctx

runDataUIWithCtxFinal
  :: forall srf fm fs msg sta a
   . DataUIWithCtx srf fm fs msg sta a
  -> DataUICtx srf fm fs
  -> DataUI srf (fm msg) (fs sta) a
runDataUIWithCtxFinal dataUiWithCtx ctx = runDataUIWithCtx (applyWrap dataUiWithCtx) ctx

applyWrap
  :: forall srf fm fs msg sta a
   . DataUIWithCtx srf fm fs msg sta a
  -> DataUIWithCtx srf fm fs (fm msg) (fs sta) a
applyWrap (DataUIWithCtx mkDataUi) = DataUIWithCtx \c@(DataUICtx ctx) -> ctx.wrap $ mkDataUi c

---

type RefineOpts a b =
  { typeName :: String
  , refine :: a -> Opt b
  , unrefine :: b -> a
  }

refineDataUIWithCtx :: forall srf fm fs msg sta a b. RefineOpts a b -> DataUIWithCtx srf fm fs msg sta a -> DataUIWithCtx srf fm fs msg sta b
refineDataUIWithCtx { typeName, refine, unrefine } (DataUIWithCtx mkDataUi) = DataUIWithCtx \ctx ->
  let
    DataUI dataUi = mkDataUi ctx
  in
    DataUI
      { extract: dataUi.extract >=> refine
      , init: lcmap (map unrefine) dataUi.init
      , name: typeName
      , update: dataUi.update
      , view: dataUi.view
      }

---

newtype DataUICtx html fm fs = DataUICtx (DataUICtxImpl html fm fs)

type DataUICtxImpl html fm fs =
  { wrap :: forall msg sta a. DataUI html msg sta a -> DataUI html (fm msg) (fs sta) a
  }

unDataUICtx :: forall html fm fs. DataUICtx html fm fs -> DataUICtxImpl html fm fs
unDataUICtx (DataUICtx impl) = impl

---

type Opt a = Either Error a

data Error = ErrNotYetDefined | ErrMsg String

derive instance Generic Error _

instance Show Error where
  show = genericShow

derive instance Newtype (DataUIWithCtx srf fm fs msg sta a) _

derive instance Newtype (DataUI srf msg sta a) _

