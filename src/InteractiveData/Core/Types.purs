module InteractiveData.Core.Types where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

type UI srf msg sta a =
    { init :: Opt a -> sta
    , update :: msg -> sta -> sta
    , view :: sta -> srf msg
    , extract :: sta -> Opt a
    , name :: String
    }

type Opt a = Either Error a

data Error = ErrNotYetDefined | ErrMsg String

derive instance Generic Error _

instance Show Error where
  show = genericShow

newtype Ctx html fm fs  = Ctx 
  { wrap :: forall msg sta a . UI html msg sta a -> UI html (fm msg) (fs sta) a
  }

newtype UICtx srf fm fs msg sta a = UICtx
  (Ctx srf fm fs -> UI srf msg sta a)
    

derive instance Newtype (UICtx srf fm fs msg sta a) _