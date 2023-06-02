module InteractiveData.Core.Record.Init where


import Prelude

import Data.Symbol (class IsSymbol)
import InteractiveData.Core.Types (Opt)
import InteractiveData.TestTypes (S1, S2, S3, T1, T2, T3)
import MVC.Record (RecordState(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

class InitRecord inits r rsta | inits -> r rsta where
  initRecord :: Record inits -> Opt (Record r) -> RecordState rsta

instance
  ( InitRecordRL rl inits r rsta
  , RowToList inits rl
  ) =>
  InitRecord inits r rsta
  where
  initRecord inits = initRecordRL prxRl inits
    where
    prxRl = Proxy :: _ rl

---

class InitRecordRL :: RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class InitRecordRL rl inits r rsta | rl inits -> r rsta where
  initRecordRL :: Proxy rl -> Record inits -> Opt (Record r) -> RecordState rsta

instance InitRecordRL RL.Nil inits r () where
  initRecordRL _ _ _ = RecordState {}

instance
  ( InitRecordRL rl' inits r rsta'
  , Row.Cons sym (Opt a -> sta) initsx inits
  , Row.Cons sym a rx r
  , Row.Cons sym sta rsta' rsta
  , Row.Lacks sym rsta'
  , IsSymbol sym
  ) =>
  InitRecordRL (RL.Cons sym x rl') inits r rsta
  where
  initRecordRL _ inits optRec =
    overRecordState update tail
    where
    update :: Record rsta' -> Record rsta
    update = Record.insert prxSym head

    overRecordState f (RecordState rec) = RecordState $ f rec

    head :: sta
    head = init (map (Record.get prxSym) optRec)

    tail :: RecordState rsta'
    tail = initRecordRL prxRl' inits optRec

    init = Record.get prxSym inits

    prxSym = Proxy :: _ sym
    prxRl' = Proxy :: _ rl'

---

t1
  :: Record
       ( field1 :: Opt T1 -> S1
       , field2 :: Opt T2 -> S2
       , field3 :: Opt T3 -> S3
       )
  -> Opt $ Record
       ( field1 :: T1
       , field2 :: T2
       , field3 :: T3
       )
  -> RecordState
       ( field1 :: S1
       , field2 :: S2
       , field3 :: S3
       )
t1 = initRecord