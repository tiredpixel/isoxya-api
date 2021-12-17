{-# LANGUAGE TemplateHaskell #-}


module Main (main) where


import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Chan
import           Control.Lens                   (makeLenses)
import           Data.Version                   (showVersion)
import           Isoxya.API
import           Isoxya.DB.Migration
import           Paths_isoxya_api               (version)
import           Snap.Snaplet
import           System.IO
import           TiredPixel.Common.Snap.Main    as S
import qualified Isoxya.Crawler                 as Crawler
import qualified Isoxya.Msg                     as M
import qualified Isoxya.Processor               as Processor
import qualified Isoxya.Streamer                as Streamer
import qualified TiredPixel.Common.Log          as L
import qualified TiredPixel.Common.Net          as N
import qualified TiredPixel.Common.SQLite.Conn  as D
import qualified TiredPixel.Common.SQLite.Meta  as D
import qualified TiredPixel.Common.SQLite.Query as D


newtype App = App {
    _api :: Snaplet API}

makeLenses ''App

main :: IO ()
main = do
    hPutStrLn stderr $ "Isoxya API " <> toString ver
    done <- S.init
    tId <- forkIO $ L.withLog $ \l -> do
        n <- N.openConn
        mCrwl <- newChan
        mProc <- newChan
        mStrm <- newChan
        D.withConnS $ \d -> do
            D.setForeignKeys True d
            D.migrate migrations d
            _ <- forkIO $ M.rx mCrwl $ Crawler.process l ver mProc n d
            _ <- forkIO $ M.rx mProc $ Processor.process l mStrm mCrwl n d
            _ <- forkIO $ M.rx mStrm $ Streamer.process l n d
            serveSnaplet S.config $ initApp mCrwl d
    S.wait done tId
    where
        ver = toText $ showVersion version


initApp :: M.ChanCrawler -> D.Conn -> SnapletInit App App
initApp mCrwl d = makeSnaplet "App" "" Nothing $ do
    api' <- nestSnaplet "" api $ initAPI mCrwl d
    return $ App api'
