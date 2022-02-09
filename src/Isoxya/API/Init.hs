module Isoxya.API.Init (
    initAPI,
    routesAPI,
    ) where


import qualified Isoxya.API.Endpoint.Apex        as Apx
import qualified Isoxya.API.Endpoint.Crawl       as Crl
import qualified Isoxya.API.Endpoint.Processor   as Pro
import qualified Isoxya.API.Endpoint.Site        as St
import qualified Isoxya.API.Endpoint.Streamer    as Str
import           Isoxya.API.Type
import qualified Isoxya.Msg                      as M
import           Snap.Core
import           Snap.Snaplet
import qualified TiredPixel.Common.SQLite.Conn   as D
import           TiredPixel.Common.Snap.CoreUtil


initAPI :: M.ChanCrawler -> D.Conn -> SnapletInit b API
initAPI mCrl d = makeSnaplet "API" "" Nothing $ do
    addRoutes routesAPI
    return $ API mCrl d

routesAPI :: [(ByteString, Handler b API ())]
routesAPI = [
    ("",                               ifTop         Apx.apex),
    --
    ("processor",                      method GET    Pro.list),
    ("processor",                      method POST   Pro.create),
    ("processor/:_",                                 notFound),
    ("processor/:processor_id",        method DELETE Pro.delete),
    ("processor/:processor_id",        method GET    Pro.read),
    ("processor/:processor_id/:_",                   notFound),
    ("site",                           method POST   St.create),
    ("site/:_",                                      notFound),
    ("site/:site_id",                  method GET    St.read),
    ("site/:site_id/:_",                             notFound),
    ("site/:site_id/crawl",            method GET    Crl.listWithSite),
    ("site/:site_id/crawl",            method POST   Crl.createWithSite),
    ("site/:site_id/crawl/:_",                       notFound),
    ("site/:site_id/crawl/:site_v",    method GET    Crl.readWithSite),
    ("site/:site_id/crawl/:site_v/:_",               notFound),
    ("streamer",                       method GET    Str.list),
    ("streamer",                       method POST   Str.create),
    ("streamer/:_",                                  notFound),
    ("streamer/:streamer_id",          method DELETE Str.delete),
    ("streamer/:streamer_id",          method GET    Str.read),
    ("streamer/:streamer_id/:_",                     notFound),
    --
    ("",                                             notFound)]
