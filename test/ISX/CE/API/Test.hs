module ISX.CE.API.Test (
    module ISX.CE.API,
    module ISX.CE.API.Core,
    module TPX.Com.Snap.Test,
    snapAPI,
    ) where


import           Control.Concurrent.Chan
import           ISX.CE.API
import           ISX.CE.API.Core         hiding (addHeader, setContentType, setHeader, (.=))
import           ISX.CE.DB.Migration
import           TPX.Com.Snap.Test
import qualified TPX.Com.SQLite.Conn     as D
import qualified TPX.Com.SQLite.Meta     as D
import qualified TPX.Com.SQLite.Query    as D


snapAPI :: SpecWith (SnapHspecState API) -> Spec
snapAPI = snap (route routesAPI) initAPITest


initAPITest :: SnapletInit b API
initAPITest = makeSnaplet "API" "" Nothing $ do
    d <- liftIO D.openConnS
    liftIO $ D.setForeignKeys True d
    liftIO $ D.migrate migrations d
    mChCrwl <- liftIO newChan
    addRoutes routesAPI
    return $ API mChCrwl d
