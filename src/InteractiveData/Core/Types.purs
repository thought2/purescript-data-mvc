module InteractiveData.Core.Types where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Profunctor (lcmap)
import Data.Show.Generic (genericShow)
import MVC.Types as MVC

type Extract sta a = sta -> Opt a

type Update msg sta = msg -> sta -> sta

type Init sta a = Opt a -> sta

type View :: forall k. (k -> Type) -> k -> Type -> Type
type View srf msg sta = sta -> srf msg


---

newtype DataUiItf srf msg sta a =
  DataUiItf
    { init :: Opt a -> sta
    , update :: msg -> sta -> sta
    , view :: sta -> srf msg
    , extract :: sta -> Opt a
    , name :: String
    }

dataUItoUI :: forall html msg sta a. DataUiItf html msg sta a -> MVC.UI html msg sta
dataUItoUI (DataUiItf dataUi) =
  { init: dataUi.init $ Left ErrNotYetDefined
  , update: dataUi.update
  , view: dataUi.view
  }

---

newtype DataUI srf fm fs msg sta a =
  DataUI
    (DataUICtx srf fm fs -> DataUiItf srf msg sta a)

runDataUIWithCtx
  :: forall srf fm fs msg sta a
   . DataUIWithCtx srf fm fs msg sta a
  -> DataUICtx srf fm fs
  -> DataUiItf srf msg sta a
runDataUIWithCtx (DataUIWithCtx dataUi) ctx = dataUi ctx

runDataUIWithCtxFinal
  :: forall srf fm fs msg sta a
   . DataUIWithCtx srf fm fs msg sta a
  -> DataUICtx srf fm fs
  -> DataUiItf srf (fm msg) (fs sta) a
runDataUIWithCtxFinal dataUiWithCtx ctx = runDataUIWithCtx (applyWrap dataUiWithCtx) ctx

applyWrap
  :: forall srf fm fs msg sta a
   . DataUIWithCtx srf fm fs msg sta a
  -> DataUIWithCtx srf fm fs (fm msg) (fs sta) a
applyWrap (DataUIWithCtx mkDataUi) = DataUIWithCtx \c@(DataUICtx ctx) -> ctx.wrap $ mkDataUi c



-- applyUI
--   :: forall html msg1 msg2 sta1 sta2 a1 a2
--    . { extract :: Extract sta1 a1 -> Extract sta2 a2
--      , init :: Init sta1 a1 -> Init sta2 a2
--      , name :: String
--      , update :: Update msg1 sta1 -> Update msg2 sta2
--      , view :: View html msg1 sta1 -> View html msg2 sta2
--      }
--   -> DataUI html msg1 sta1 a1
--   -> DataUI html msg2 sta2 a2
-- applyUI r ui1 = C.UICtx \ctx ->
--   { name: r.name
--   , view:
--       r.view (runDataUI ctx ui1).view
--   , extract:
--       r.extract (runDataUI ctx ui1).extract
--   , update:
--       r.update (runDataUI ctx ui1).update
--   , init:
--       r.init (runDataUI ctx ui1).init
--   }

-- applyUI2
--   :: forall html msg1 msg2 msg3 sta1 sta2 sta3 a1 a2 a3
--    . { extract :: Extract sta1 a1 -> Extract sta2 a2 -> Extract sta3 a3
--      , init :: Init sta1 a1 -> Init sta2 a2 -> Init sta3 a3
--      , name :: String
--      , update :: Update msg1 sta1 -> Update msg2 sta2 -> Update msg3 sta3
--      , view :: View html msg1 sta1 -> View html msg2 sta2 -> View html msg3 sta3
--      }
--   -> DataUI html msg1 sta1 a1
--   -> DataUI html msg2 sta2 a2
--   -> DataUI html msg3 sta3 a3
-- applyUI2 r ui1 ui2 = C.UICtx \ctx ->
--   { name: r.name
--   , view:
--       r.view (runDataUI ctx ui1).view (runDataUI ctx ui2).view
--   , extract:
--       r.extract (runDataUI ctx ui1).extract (runDataUI ctx ui2).extract
--   , update:
--       r.update (runDataUI ctx ui1).update (runDataUI ctx ui2).update
--   , init:
--       r.init (runDataUI ctx ui1).init (runDataUI ctx ui2).init
--   }
---

type RefineOpts a b =
  { typeName :: String
  , refine :: a -> Opt b
  , unrefine :: b -> a
  }

refineDataUIWithCtx :: forall srf fm fs msg sta a b. RefineOpts a b -> DataUIWithCtx srf fm fs msg sta a -> DataUIWithCtx srf fm fs msg sta b
refineDataUIWithCtx { typeName, refine, unrefine } (DataUIWithCtx mkDataUi) = DataUIWithCtx \ctx ->
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
  { wrap :: forall msg sta a. DataUiItf html msg sta a -> DataUiItf html (fm msg) (fs sta) a
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

derive instance Newtype (DataUiItf srf msg sta a) _

