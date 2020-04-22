module Web.Fox.Request
  ()
where

import           Network.Wai                    ( Request
                                                , requestMethod
                                                , pathInfo
                                                , Response
                                                , ResponseReceived
                                                )
import           Network.HTTP.Types
import           Web.Fox.Types
import qualified Data.Text                     as T
import           Data.Maybe                     ( isJust )
import           Web.Fox.Route                  ( parseRoutePath )
import           Control.Lens                   ( (.~)
                                                , (&)
                                                , view
                                                , (^.)
                                                )
import           Web.Fox.Response
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Reader
import qualified Data.Map                      as M
import           Data.Functor                   ( ($>) )

incomingGet :: Request -> FoxT IO ()
incomingGet r = do
  let path = pathInfo r
  pure ()

getRoute :: Path -> Method -> [Route] -> Maybe (Route, Params)
getRoute _   _ []       = Nothing
getRoute uri m (r : rs) = case parseRoutePath uri r of
  Nothing -> getRoute uri m rs
  Just ps -> if m == r ^. rMethod then Just (r, ps) else getRoute uri m rs

incoming :: Request -> (Response -> IO ResponseReceived) -> FoxT IO ()
incoming req res = do
  rs <- getRoutes <$> view routes
  let ctx = Ctx req M.empty res
  lift $ runReaderT (handleIncoming rs) ctx

handleIncoming :: [Route] -> ActionT IO ()
handleIncoming rs = do
  req' <- view req
  let path = pathInfo req'
  case getRoute (T.intercalate "/" path) (requestMethod req') rs of
    Nothing      -> send (text "Not found" <> e404) $> ()
    Just (r, ps) -> local (params .~ ps) (r ^. rHandler) $> ()



