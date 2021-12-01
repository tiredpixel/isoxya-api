module Isoxya.Streamer.Processor (process) where


import           Data.Aeson
import           Isoxya.API.Href
import           TiredPixel.Common.Isoxya.Streamer
import           TiredPixel.Common.Snap.CoreUtil
import           TiredPixel.Common.URI
import qualified Isoxya.DB                         as D
import qualified Isoxya.Msg                        as M
import qualified Network.HTTP.Conduit              as HTTP
import qualified Network.HTTP.Types                as HTTP
import qualified TiredPixel.Common.Log             as L
import qualified TiredPixel.Common.Net             as N


process :: L.Logger -> N.Conn -> D.Conn -> M.MsgStreamer -> IO ()
process l n d (strmId, msg) = do
    Just strm <- D.rStreamer strmId d
    L.debug l $ show strm
    Just proc <- D.rProcessor (M.crawlPageDataProcessorId msg) d
    L.debug l $ show proc
    Just crwl <- D.rCrawl
        (M.crawlPageDataSiteId msg, M.crawlPageDataSiteV msg) d
    L.debug l $ show crwl
    Just site <- D.rSiteId (D.crawlSiteId crwl) d
    L.debug l $ show site
    Just page <- D.rPageId (D.crawlSiteId crwl, M.crawlPageDataPageId msg) d
    let tx = genStreamer msg proc crwl site page
    L.debug l $ show tx
    let req = N.jsonReq $
            N.makeReq' "POST" (D.unStreamerURL $ D.streamerURL strm) (encode tx)
    L.debug l $ show req
    L.debug l $ decodeUtf8 $ encode tx
    res <- N.makeRes req n
    L.debug l $ show res
    L.info l $
        decodeUtf8 (unCrawlHref $
            toRouteHref (D.siteURL site, D.crawlSiteV crwl)) <> " STRM " <>
        show (D.unStreamerId $ D.streamerId strm) <> " " <>
        show (D.unSiteURL $ D.siteURL site) <>
        show (D.unPageURL $ D.pageURL page) <> " " <>
        show (HTTP.statusCode $ HTTP.responseStatus res)


genStreamer :: M.CrawlPageData -> D.Processor -> D.Crawl -> D.Site ->
    D.Page -> Streamer
genStreamer msg proc crwl site page = Streamer {
    streamerCrawlBegan    = D.unSiteV $ D.crawlSiteV crwl,
    streamerCrawlHref     = crwlH,
    streamerData          = M.crawlPageDataData msg,
    streamerProcessorHref = procH,
    streamerProcessorTag  = D.processorTag proc,
    streamerRetrieved     = D.unPageV $ M.crawlPageDataPageV msg,
    streamerSiteHref      = decodeUtf8 $ unSiteHref $ toRouteHref (D.siteURL site),
    streamerSiteURL       = URIAbsolute $ D.unSiteURL $ D.siteURL site,
    streamerURL           = url}
    where
        crwlH = decodeUtf8 $ unCrawlHref $
            toRouteHref (D.siteURL site, D.crawlSiteV crwl)
        procH = decodeUtf8 $ unProcessorHref $
            toRouteHref (D.processorId proc)
        url = URIAbsolute $ D.unSiteURL $
            D.pageURLAbs (D.siteURL site) (D.pageURL page)
