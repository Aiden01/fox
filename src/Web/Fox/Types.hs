{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, DeriveFunctor, GADTs #-}

module Web.Fox.Types
  ( Ctx(..)
  , req
  , params
  , ActionT(..)
  , FoxT(..)
  , Route(..)
  , Group(..)
  , Handler
  , Path
  , FoxState(..)
  , routes
  , Routes(..)
  , IsRoute(..)
  , Method(..)
  , RParser
  , Params
  , rMethod
  , rPath
  , rParser
  , rHandler
  , respond
  )
where

import           Control.Monad.Reader
import           Control.Lens
import           Network.Wai
import           Data.Aeson                     ( FromJSON )
import qualified Data.Map                      as M
import           Text.Megaparsec                ( Parsec )
import           Data.Void                      ( Void )
import qualified Data.Text                     as T
import           Network.HTTP.Types

type Params = M.Map T.Text T.Text

data Ctx = Ctx
    { _req :: Request
    , _params :: Params
    , _respond :: Response -> IO ResponseReceived }

type ActionT = ReaderT Ctx

class IsRoute r where
    getRoutes :: r -> [Route]

instance IsRoute Route where
  getRoutes r = [r]

instance IsRoute Group where
  getRoutes (Group p routes) = getRoutes routes

instance IsRoute r => IsRoute [r] where
  getRoutes = concatMap getRoutes

instance IsRoute Routes where
  getRoutes (Routes rs) = getRoutes rs


data Routes where
    Routes ::IsRoute r => [r] -> Routes

data FoxState = FoxState
    { _routes :: Routes  }

type FoxT = ReaderT FoxState


type Path = T.Text
type Handler = ActionT IO ResponseReceived

type RParser = Parsec Void T.Text

data Route = Route
    { _rPath :: Path
    , _rMethod :: Method
    , _rHandler :: Handler
    , _rParser :: RParser Params }
data Group = Group Path [Route]

makeLenses ''FoxState
makeLenses ''Ctx
makeLenses ''Route
