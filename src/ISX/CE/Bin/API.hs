module Main (main) where


import           Control.Concurrent      (threadDelay)
import           Data.Version            (showVersion)
import           ISX.CE.DB.Migration
import           Paths_isx_ce            (version)
import           System.IO
import qualified TPX.Com.Log             as L
import qualified TPX.Com.SQLite.Conn     as D
import qualified TPX.Com.SQLite.Meta     as D
import qualified TPX.Com.SQLite.Query    as D


main :: IO ()
main = do
    let ver = toText $ showVersion version
    hPutStrLn stderr $ "Isoxya CE API " <> toString ver
    _ <- L.withLog $ \_ ->
        D.withConnS $ \d -> do
            D.setForeignKeys True d
            D.migrate migrations d
    threadDelay 60000000
