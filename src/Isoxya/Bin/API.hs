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
    tId <- forkIO $ do
        n <- N.openConn
        mCrl <- newChan
        mPro <- newChan
        mStr <- newChan
        D.withConnS $ \d -> do
            D.setForeignKeys True d
            D.migrate migrations schema d
            _ <- forkIO $ M.rx mCrl $ Crawler.process mPro n d
            _ <- forkIO $ M.rx mPro $ Processor.process mStr mCrl n d
            _ <- forkIO $ M.rx mStr $ Streamer.process n d
            serveSnaplet S.config $ initApp mCrl d
    S.wait done tId
    where
        ver = toText $ showVersion version


initApp :: M.ChanCrawler -> D.Conn -> SnapletInit App App
initApp mCrl d = makeSnaplet "App" "" Nothing $ do
    api' <- nestSnaplet "" api $ initAPI mCrl d
    return $ App api'
