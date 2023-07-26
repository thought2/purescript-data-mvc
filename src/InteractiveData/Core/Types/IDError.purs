module InteractiveData.Core.Types.IDError
  ( IDError(..)
  , IDErrorCase(..)
  , Opt
  , scopeError
  , scopeErrors
  , scopeOpt
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import InteractiveData.Core.Types.DataPath (DataPathSegment, DataPath)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

type Opt a = Either (NonEmptyArray IDError) a

data IDErrorCase
  = IDErrNotYetDefined
  | IDErrMsg String

data IDError = IDError DataPath IDErrorCase

--------------------------------------------------------------------------------
--- Constructors & Combinators
--------------------------------------------------------------------------------

scopeError :: DataPathSegment -> IDError -> IDError
scopeError seg (IDError path case_) = IDError ([ seg ] <> path) case_

scopeErrors :: DataPathSegment -> NonEmptyArray IDError -> NonEmptyArray IDError
scopeErrors seg = map (scopeError seg)

scopeOpt :: forall a. DataPathSegment -> Opt a -> Opt a
scopeOpt seg = lmap (scopeErrors seg)

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

derive instance Ord IDError

derive instance Eq IDError

derive instance Generic IDError _

derive instance Generic IDErrorCase _

instance Show IDError where
  show = genericShow

instance Show IDErrorCase where
  show = genericShow

derive instance Ord IDErrorCase
derive instance Eq IDErrorCase
