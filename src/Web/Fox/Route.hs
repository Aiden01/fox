{-# LANGUAGE ExistentialQuantification #-}

module Web.Fox.Route
  ( group
  , addRoute
  , parseRoutePath
  )
where

import           Data.Aeson                     ( FromJSON )
import           Data.Mod
import           Text.Megaparsec
import           Text.Megaparsec.Char           ( string
                                                , char
                                                )
import           Web.Fox.Types
import qualified Data.Text                     as T
import qualified Data.Map                      as M
import           Control.Monad                  ( foldM )
import           Data.Functor                   ( ($>) )
import qualified Data.List                     as L
import           Control.Lens                   ( (^.) )
import           Network.HTTP.Types.Method

startsWith :: T.Text -> T.Text -> Bool
startsWith t predicate = T.dropEnd (T.length predicate) t == predicate

parseSegment :: Params -> T.Text -> RParser Params
parseSegment ps r
  | r `startsWith` ":" = do
    let rName = T.tail r
    rValue <- many (noneOf ['/'])
    pure (M.insert rName (T.pack rValue) ps)
  | otherwise = (string r <* char '/') $> ps

mkRouteParser :: T.Text -> RParser Params
mkRouteParser = foldM parseSegment M.empty . L.delete "" . T.splitOn "/"

parseRoutePath :: T.Text -> Route -> Maybe Params
parseRoutePath path r =
  either (const Nothing) Just (parse (r ^. rParser) "" path)


get, put, delete, patch, post :: Path -> Handler -> Route
get p h = route p methodGet h (mkRouteParser p)
put p h = route p methodPut h (mkRouteParser p)
delete p h = route p methodDelete h (mkRouteParser p)
patch p h = route p methodPatch h (mkRouteParser p)
post p h = route p methodPost h (mkRouteParser p)

route :: Path -> Method -> Handler -> RParser Params -> Route
route = Route

group :: Path -> Mod Group -> Group
group p mod = unMod mod (Group p [])

addRoute :: Route -> Mod Group
addRoute r = Mod (\(Group p rs) -> Group p (r : rs))

