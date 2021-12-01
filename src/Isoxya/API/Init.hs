module Isoxya.API.Init (
    initAPI,
    routesAPI,
    ) where


import           Isoxya.API.Type
import           Snap.Core
import           Snap.Snaplet
import           TiredPixel.Common.Snap.CoreUtil
import qualified Isoxya.API.Endpoint.Apex        as EA
import qualified Isoxya.API.Endpoint.Crawl       as EC
import qualified Isoxya.API.Endpoint.Processor   as EProc
import qualified Isoxya.API.Endpoint.Site        as ES
import qualified Isoxya.API.Endpoint.Streamer    as EStrm
import qualified Isoxya.Msg                      as M
import qualified TiredPixel.Common.SQLite.Conn   as D


initAPI :: M.ChanCrawler -> D.Conn -> SnapletInit b API
initAPI mCrwl d = makeSnaplet "API" "" Nothing $ do
    addRoutes routesAPI
    return $ API mCrwl d

routesAPI :: [(ByteString, Handler b API ())]
routesAPI = [
    ("",                               ifTop         EA.apex),
    --
    ("processor",                      method GET    EProc.list),
    ("processor",                      method POST   EProc.create),
    ("processor/:_",                                 notFound),
    ("processor/:processor_id",        method DELETE EProc.delete),
    ("processor/:processor_id",        method GET    EProc.read),
    ("processor/:processor_id/:_",                   notFound),
    ("site",                           method POST   ES.create),
    ("site/:_",                                      notFound),
    ("site/:site_id",                  method GET    ES.read),
    ("site/:site_id/:_",                             notFound),
    ("site/:site_id/crawl",            method GET    EC.listSite),
    ("site/:site_id/crawl",            method POST   EC.createSite),
    ("site/:site_id/crawl/:_",                       notFound),
    ("site/:site_id/crawl/:site_v",    method GET    EC.readSite),
    ("site/:site_id/crawl/:site_v/:_",               notFound),
    ("streamer",                       method GET    EStrm.list),
    ("streamer",                       method POST   EStrm.create),
    ("streamer/:_",                                  notFound),
    ("streamer/:streamer_id",          method DELETE EStrm.delete),
    ("streamer/:streamer_id",          method GET    EStrm.read),
    ("streamer/:streamer_id/:_",                     notFound),
    --
    ("",                                             notFound)]
