module Isoxya.Streamer.Processor (process) where


import           Data.Aeson
import           Isoxya.API.Href
import qualified Isoxya.DB                         as D
import qualified Isoxya.Msg                        as M
import qualified Network.HTTP.Conduit              as HTTP
import qualified Network.HTTP.Types                as HTTP
import           TiredPixel.Common.Isoxya.Streamer
import           TiredPixel.Common.Logger
import qualified TiredPixel.Common.Net             as N
import           TiredPixel.Common.Snap.CoreUtil
import           TiredPixel.Common.URI


process :: N.Conn -> D.Conn -> M.MsgStreamer -> IO ()
process n d (strId, msg) = runLogger $ do
    Just str <- D.rStreamer strId d
    logDebugN $ show str
    Just pro <- D.rProcessor (M.crawlPageDataProcessorId msg) d
    logDebugN $ show pro
    Just crl <- D.rCrawl (M.crawlPageDataSiteId msg, M.crawlPageDataSiteV msg) d
    logDebugN $ show crl
    Just st <- D.rSiteId (D.crawlSiteId crl) d
    logDebugN $ show st
    Just pg <- D.rPageId (D.crawlSiteId crl, M.crawlPageDataPageId msg) d
    let tx = genStreamer msg pro crl st pg
    logDebugN $ show tx
    let req = N.jsonReq $ N.makeReq' "POST"
            (D.unStreamerURL $ D.streamerURL str) (encode tx)
    logDebugN $ show req
    logDebugN $ decodeUtf8 $ encode tx
    res <- N.makeRes req n
    logDebugN $ show res
    logInfoN $
        decodeUtf8 (unCrawlHref $
            toRouteHref (D.siteURL st, D.crawlSiteV crl)) <> " STR " <>
        show (D.unStreamerId $ D.streamerId str) <> " " <>
        show (D.unSiteURL $ D.siteURL st) <>
        show (D.unPageURL $ D.pageURL pg) <> " " <>
        show (HTTP.statusCode $ HTTP.responseStatus res)


genStreamer :: M.CrawlPageData -> D.Processor -> D.Crawl -> D.Site -> D.Page ->
    Streamer
genStreamer msg pro crl st pg = Streamer {
    streamerCrawlBegan    = D.unSiteV $ D.crawlSiteV crl,
    streamerCrawlHref     = crlH,
    streamerData          = M.crawlPageDataData msg,
    streamerProcessorHref = proH,
    streamerProcessorTag  = D.processorTag pro,
    streamerRetrieved     = D.unPageV $ M.crawlPageDataPageV msg,
    streamerSiteHref      = stH,
    streamerSiteURL       = URIAbsolute $ D.unSiteURL $ D.siteURL st,
    streamerURL           = url}
    where
        stH = decodeUtf8 $ unSiteHref $ toRouteHref (D.siteURL st)
        crlH = decodeUtf8 $ unCrawlHref $
            toRouteHref (D.siteURL st, D.crawlSiteV crl)
        proH = decodeUtf8 $ unProcessorHref $
            toRouteHref (D.processorId pro)
        url = URIAbsolute $ D.unSiteURL $
            D.pageURLAbs (D.siteURL st) (D.pageURL pg)
