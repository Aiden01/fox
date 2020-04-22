module Web.Fox.Action
  ( param
  )
where

import           Control.Lens
import           Web.Fox.Types
import qualified Data.Map                      as M
import qualified Data.Text                     as T

param :: Read a => T.Text -> ActionT IO (Maybe a)
param p = do
  p' <- M.lookup p <$> view params
  pure ((read . T.unpack) <$> p')
