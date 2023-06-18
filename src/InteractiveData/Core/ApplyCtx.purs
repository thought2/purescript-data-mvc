module InteractiveData.Core.ApplyCtx where

import Data.Symbol (class IsSymbol)
import InteractiveData.Core.Types (DataUI, DataUICtx, DataUiItf, applyWrap, runDataUi)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

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

