module Isoxya.API.Test (
    module Isoxya.API,
    module Isoxya.API.Core,
    module TiredPixel.Common.Snap.Test,
    snapAPI,
    ) where


import           Control.Concurrent.Chan
import           Isoxya.API
import           Isoxya.API.Core                hiding (addHeader, setContentType, setHeader, (.=))
import           Isoxya.DB.Migration
import           TiredPixel.Common.Snap.Test
import qualified TiredPixel.Common.SQLite.Conn  as D
import qualified TiredPixel.Common.SQLite.Meta  as D
import qualified TiredPixel.Common.SQLite.Query as D


snapAPI :: SpecWith (SnapHspecState API) -> Spec
snapAPI = snap (route routesAPI) initAPITest


initAPITest :: SnapletInit b API
initAPITest = makeSnaplet "API" "" Nothing $ do
    d <- liftIO D.openConnS
    liftIO $ D.setForeignKeys True d
    liftIO $ D.migrate migrations d
    mCrwl <- liftIO newChan
    addRoutes routesAPI
    return $ API mCrwl d
