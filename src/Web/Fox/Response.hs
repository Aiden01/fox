module Web.Fox.Response
  ( text
  , send
  , res
  , status
  , e404
  )
where

import qualified Data.Text                     as T
import           Web.Fox.Types
import           Network.Wai                    ( responseLBS
                                                , Response
                                                )
import           Network.HTTP.Types
import           Control.Lens
import           Data.ByteString.Lazy
import           Control.Monad.Trans            ( liftIO )
import           Data.Mod


data ResponseBuilder = ResponseBuilder
  { _rData :: ByteString
  , _rHeaders :: [Header]
  , _rStatus :: Status }
makeLenses ''ResponseBuilder

emptyResponse :: ResponseBuilder
emptyResponse = ResponseBuilder "" [] status200

send :: Mod ResponseBuilder -> Handler
send mod = res
  (responseLBS (builder ^. rStatus) (builder ^. rHeaders) (builder ^. rData))
  where builder = unMod mod emptyResponse

res :: Response -> Handler
res res' = view respond >>= liftIO . ($ res')

text :: ByteString -> Mod ResponseBuilder
text = Mod . set rData

status :: Status -> Mod ResponseBuilder
status = Mod . set rStatus

e404 :: Mod ResponseBuilder
e404 = status status404
