module ISX.CE.API.Init (
    initAPI,
    routesAPI,
    ) where


import           ISX.CE.API.Types
import           Snap.Core
import           Snap.Snaplet
import           TPX.Com.Snap.CoreUtils
import qualified ISX.CE.API.Zone.Apex   as ZA
import qualified TPX.Com.SQLite.Conn    as D


initAPI :: D.Conn -> SnapletInit b API
initAPI d = makeSnaplet "API" "" Nothing $ do
    addRoutes routesAPI
    return $ API d

routesAPI :: [(ByteString, Handler b API ())]
routesAPI = [
    ("",                                    ifTop           ZA.apex),
    --
    --
    ("",                                                    notFound)]
