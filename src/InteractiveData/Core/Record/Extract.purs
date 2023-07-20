module InteractiveData.Core.Record.Extract where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import InteractiveData.Core.Types (DataPathSegment(..), DataPathSegmentField(..), Opt, scopeOpt)
import InteractiveData.TestTypes (S1, S2, S3, T1, T2, T3)
import MVC.Record (RecordState(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

class ExtractRecord extracts rsta r | extracts -> rsta r where
  extractRecord :: Record extracts -> RecordState rsta -> Opt (Record r)

instance
  ( RowToList extracts rl
  , ExtractRecordRL rl extracts rsta r
  ) =>
  ExtractRecord extracts rsta r
  where
  extractRecord :: Record extracts -> RecordState rsta -> Opt (Record r)
  extractRecord = extractRecordRL prxRl
    where
    prxRl = Proxy :: _ rl

---

class ExtractRecordRL :: RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class ExtractRecordRL rl extracts rsta r | rl extracts -> rsta r where
  extractRecordRL :: Proxy rl -> Record extracts -> RecordState rsta -> Opt (Record r)

instance ExtractRecordRL RL.Nil extracts rsta ()
  where
  extractRecordRL :: Proxy RL.Nil -> Record extracts -> RecordState rsta -> Opt (Record ())
  extractRecordRL _ _ _ = Right {}

instance
  ( ExtractRecordRL rl' extracts rsta r'
  , Row.Cons sym (sta -> Opt a) extractsx extracts
  , Row.Cons sym sta rstax rsta
  , Row.Cons sym a r' r
  , Row.Lacks sym r'
  , IsSymbol sym
  ) =>
  ExtractRecordRL (RL.Cons sym x rl') extracts rsta r
  where
  extractRecordRL :: Proxy (RL.Cons sym x rl') -> Record extracts -> RecordState rsta -> Opt (Record r)
  extractRecordRL _ extracts (RecordState states) =
    case head, tail of
      Left errors1, Left errors2 ->
        Left (errors1 <> errors2)

      Left errors, Right _ ->
        Left errors

      Right _, Left errors ->
        Left errors

      Right x, Right xs ->
        Right $ Record.insert prxSym x xs

    where
    tail :: Opt (Record r')
    tail = extractRecordRL prxRl' extracts (RecordState states)

    head :: Opt a
    head = extract state # scopeOpt pathSeg

    pathSeg :: DataPathSegment
    pathSeg = SegField $ SegStaticKey (reflectSymbol prxSym)

    extract :: sta -> Opt a
    extract = Record.get prxSym extracts

    state :: sta
    state = Record.get prxSym states

    prxRl' = Proxy :: _ rl'
    prxSym = Proxy :: _ sym

---

t1
  :: Record
       ( field1 :: S1 -> Opt T1
       , field2 :: S2 -> Opt T2
       , field3 :: S3 -> Opt T3
       )
  -> RecordState
       ( field1 :: S1
       , field2 :: S2
       , field3 :: S3
       )
  -> Opt $ Record
       ( field1 :: T1
       , field2 :: T2
       , field3 :: T3
       )
t1 = extractRecord