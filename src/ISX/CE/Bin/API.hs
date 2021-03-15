{-# LANGUAGE TemplateHaskell #-}


module Main (main) where


import           Control.Concurrent   (forkIO)
import           Control.Lens         (makeLenses)
import           Data.Version         (showVersion)
import           ISX.CE.API
import           ISX.CE.DB.Migration
import           Paths_isx_ce         (version)
import           Snap.Snaplet
import           System.IO
import           TPX.Com.Snap.Main    as S
import qualified TPX.Com.Log          as L
import qualified TPX.Com.SQLite.Conn  as D
import qualified TPX.Com.SQLite.Meta  as D
import qualified TPX.Com.SQLite.Query as D


newtype App = App {
    _api :: Snaplet API}

makeLenses ''App

main :: IO ()
main = do
    let ver = toText $ showVersion version
    hPutStrLn stderr $ "Isoxya CE API " <> toString ver
    done <- S.init
    tId <- forkIO $ L.withLog $ \_ ->
        D.withConnS $ \d -> do
            D.setForeignKeys True d
            D.migrate migrations d
            serveSnaplet S.config $ initApp d
    S.wait done tId


initApp :: D.Conn -> SnapletInit App App
initApp d = makeSnaplet "App" "" Nothing $ do
    api' <- nestSnaplet "" api $ initAPI d
    return $ App api'
