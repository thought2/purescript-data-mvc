module InteractiveData.Core.Variant.Init where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import InteractiveData.Core.Types (Opt)
import InteractiveData.TestTypes (S1, S2, S3, T1, T2, T3)
import MVC.Variant.Types (VariantState(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class InitVariant :: Row Type -> Symbol -> Row Type -> Row Type -> Constraint
class InitVariant inits initsym r rsta | inits initsym -> r rsta where
  initVariant :: Record inits -> Proxy initsym -> Maybe (Variant r) -> VariantState rsta

instance
  ( Row.Cons initsym (Maybe a -> sta) initsx inits
  , Row.Cons initsym sta rstax rsta
  , IsSymbol initsym
  , RowToList inits rl
  , InitVariantRL rl inits r rsta
  ) =>
  InitVariant inits initsym r rsta where
  initVariant inits _ = case _ of
    Nothing -> VariantState $ V.inj prxInitSym state
    Just va -> initVariantRL prxRl inits va
    where
    state :: sta
    state = init Nothing

    init :: Maybe a -> sta
    init = Record.get prxInitSym inits

    prxInitSym = Proxy :: _ initsym
    prxRl = Proxy :: _ rl

---

class InitVariantRL :: RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class InitVariantRL rl inits r rsta | rl inits -> r rsta where
  initVariantRL :: Proxy rl -> Record inits -> Variant r -> VariantState rsta

instance InitVariantRL RL.Nil inits () rsta where
  initVariantRL _ _ = V.case_

instance
  ( InitVariantRL rl' inits r' rsta
  , Row.Cons sym a r' r
  , Row.Cons sym (Maybe a -> sta) initsx inits
  , Row.Cons sym sta rstax rsta
  , IsSymbol sym
  ) =>
  InitVariantRL (RL.Cons sym x rl') inits r rsta where
  initVariantRL _ inits =
    tail
      # V.on prxSym (Just >>> init >>> V.inj prxSym >>> VariantState)
    where
    init :: Maybe a -> sta
    init = Record.get prxSym inits

    tail :: Variant r' -> VariantState rsta
    tail = initVariantRL prxRl' inits

    prxRl' = Proxy :: _ rl'
    prxSym = Proxy :: _ sym

---

testInitVariant
  :: Record
       ( case1 :: Maybe T1 -> S1
       , case2 :: Maybe T2 -> S2
       , case3 :: Maybe T3 -> S3
       )
  -> Proxy "case2"
  -> Maybe
       ( Variant
           ( case1 :: T1
           , case2 :: T2
           , case3 :: T3
           )
       )
  -> VariantState
       ( case1 :: S1
       , case2 :: S2
       , case3 :: S3
       )
testInitVariant = initVariant