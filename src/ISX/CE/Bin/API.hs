{-# LANGUAGE TemplateHaskell #-}


module Main (main) where


import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan
import           Control.Lens            (makeLenses)
import           Data.Version            (showVersion)
import           ISX.CE.API
import           ISX.CE.DB.Migration
import           Paths_isx_ce            (version)
import           Snap.Snaplet
import           System.IO
import           TPX.Com.Snap.Main       as S
import qualified ISX.CE.Crwl             as Crwl
import qualified ISX.CE.Msg              as M
import qualified ISX.CE.Proc             as Proc
import qualified ISX.CE.Strm             as Strm
import qualified TPX.Com.Log             as L
import qualified TPX.Com.Net             as N
import qualified TPX.Com.SQLite.Conn     as D
import qualified TPX.Com.SQLite.Meta     as D
import qualified TPX.Com.SQLite.Query    as D


newtype App = App {
    _api :: Snaplet API}

makeLenses ''App

main :: IO ()
main = do
    let ver = toText $ showVersion version
    hPutStrLn stderr $ "Isoxya CE API " <> toString ver
    done <- S.init
    tId <- forkIO $ L.withLog $ \l -> do
        n <- N.openConn
        mChCrwl <- newChan
        mChProc <- newChan
        mChStrm <- newChan
        D.withConnS $ \d -> do
            D.setForeignKeys True d
            D.migrate migrations d
            _ <- forkIO $ M.rx mChCrwl $ Crwl.process l ver mChProc n d
            _ <- forkIO $ M.rx mChProc $ Proc.process l mChStrm mChCrwl n d
            _ <- forkIO $ M.rx mChStrm $ Strm.process l n d
            serveSnaplet S.config $ initApp mChCrwl d
    S.wait done tId


initApp :: M.ChanCrwl -> D.Conn -> SnapletInit App App
initApp mChCrwl d = makeSnaplet "App" "" Nothing $ do
    api' <- nestSnaplet "" api $ initAPI mChCrwl d
    return $ App api'
