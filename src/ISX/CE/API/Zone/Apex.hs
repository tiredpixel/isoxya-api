module ISX.CE.API.Zone.Apex (
    apex,
    ) where


import           Data.Time.Clock
import           Data.Version        (showVersion)
import           ISX.CE.API.Core
import           Paths_isx_ce        (version)
import qualified TPX.Com.SQLite.Conn as D


apex :: Handler b API ()
apex = do
    d <- gets _db
    t <- liftIO getCurrentTime
    let v = toText $ showVersion version
    _ <- D.ping d
    writeJSON $ Apex t v
