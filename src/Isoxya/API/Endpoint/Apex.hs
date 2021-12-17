module Isoxya.API.Endpoint.Apex (
    apex,
    ) where


import           Data.Time.Clock
import           Data.Version                  (showVersion)
import           Isoxya.API.Core
import           Paths_isoxya_api              (version)
import qualified TiredPixel.Common.SQLite.Conn as D


apex :: Handler b API ()
apex = do
    d <- gets _db
    t <- liftIO getCurrentTime
    _ <- D.ping d
    writeJSON $ Apex t ver
    where
        ver = toText $ showVersion version
