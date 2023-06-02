module InteractiveData.Core.Record.Extract where

import Data.Either (Either)
import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import InteractiveData.Core.Types (Error, Opt)
import InteractiveData.TestTypes (S1, S2, S3, T1, T2, T3)
import MVC.Record (RecordState(..))
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record as Record
import Record.Extra (class SequenceRecord, sequenceRecord)
import Type.Function (type ($))
import Type.Proxy (Proxy)

class ExtractRecord extracts rsta r | extracts -> rsta r where
  extractRecord :: Record extracts -> RecordState rsta -> Opt (Record r)

instance
  ( HMapWithIndex (F rsta) (Record extracts) (Record rr)
  , RowToList rr rl
  , SequenceRecord rl rr () r (Either Error)
  ) =>
  ExtractRecord extracts rsta r where
  extractRecord extracts rsta = sequenceRecord rr
    where
    rr = hmapWithIndex (F rsta) extracts

data F rsta = F (RecordState rsta)

instance
  ( Row.Cons sym sta rstax rsta
  , IsSymbol sym
  ) =>
  MappingWithIndex (F rsta) (Proxy sym) (sta -> Opt a) (Opt a) where
  mappingWithIndex (F (RecordState rsta)) prxSym f = f state
    where
    state = Record.get prxSym rsta

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