module Web.Fox
 (

 )
where

import Network.Wai.Handler.Warp (Port)
import Web.Fox.Types 

fox :: Port -> Routes -> IO ()
fox p routes = undefined
    where
        state = FoxState routes