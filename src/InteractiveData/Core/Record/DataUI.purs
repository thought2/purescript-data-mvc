module InteractiveData.Core.Record.DataUI
  ( class DataUiRecord
  , dataUiRecord
  ) where

import InteractiveData.Core.ApplyCtx (class ApplyCtx, mapApplyCtx)
import InteractiveData.Core.Record.DataUiItf (class DataUiItfRecord, dataUiItfRecord)
import InteractiveData.Core.Types.DataUI (DataUI(..), DataUICtx)
import MVC.Record (RecordMsg, RecordState)
import MVC.Record.UI (UIRecordProps)

class
  DataUiRecord
    (datauis :: Row Type)
    (fm :: Type -> Type)
    (fs :: Type -> Type)
    (srf :: Type -> Type)
    (rmsg :: Row Type)
    (rsta :: Row Type)
    (r :: Row Type)
  | datauis -> fm fs srf rmsg rsta r
  where
  dataUiRecord
    :: UIRecordProps srf (RecordMsg rmsg) (RecordState rsta)
    -> Record datauis
    -> DataUI srf fm fs (RecordMsg rmsg) (RecordState rsta) (Record r)

instance
  ( ApplyCtx (DataUICtx srf fm fs) datauis uis
  , DataUiItfRecord uis srf rmsg rsta r
  ) =>
  DataUiRecord datauis fm fs srf rmsg rsta r
  where
  dataUiRecord
    :: UIRecordProps srf (RecordMsg rmsg) (RecordState rsta)
    -> Record datauis
    -> DataUI srf fm fs (RecordMsg rmsg) (RecordState rsta) (Record r)
  dataUiRecord props datauis = DataUI \ctx ->
    let
      uis :: Record uis
      uis = mapApplyCtx ctx datauis
    in
      dataUiItfRecord props uis

