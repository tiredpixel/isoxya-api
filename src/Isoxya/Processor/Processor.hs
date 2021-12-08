module Isoxya.Processor.Processor (process) where


import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Isoxya.API.Href
import           TiredPixel.Common.Isoxya.Processor
import           TiredPixel.Common.Snap.CoreUtil
import           TiredPixel.Common.URI
import qualified Data.Map                           as M
import qualified Data.Set                           as S
import qualified Isoxya.DB                          as D
import qualified Isoxya.Msg                         as M
import qualified Network.HTTP.Conduit               as HTTP
import qualified Network.HTTP.Types                 as HTTP
import qualified TiredPixel.Common.Log              as L
import qualified TiredPixel.Common.Net              as N


process :: L.Logger -> M.ChanStreamer -> M.ChanCrawler -> N.Conn -> D.Conn ->
    M.MsgProcessor -> IO ()
process l mStrm mCrwl n d (procId, msg) = do
    Just proc <- D.rProcessor procId d
    L.debug l $ show proc
    Just crwl <- D.rCrawl (M.crawlPageSiteId msg, M.crawlPageSiteV msg) d
    L.debug l $ show crwl
    Just site <- D.rSiteId (D.crawlSiteId crwl) d
    L.debug l $ show site
    Just page <- D.rPageId (D.crawlSiteId crwl, M.crawlPagePageId msg) d
    let tx = genProcessorI msg proc crwl site page
    L.debug l $ show tx
    let req = N.jsonReq $ N.makeReq' "POST"
            (D.unProcessorURL $ D.processorURL proc) (encode tx)
    L.debug l $ show req
    L.debug l $ decodeUtf8 $ encode tx
    res <- N.makeRes req n
    L.debug l $ show res
    L.debug l $ decodeUtf8 $ HTTP.responseBody res
    let Just rx = decode $ HTTP.responseBody res :: Maybe ProcessorO
    pageIdsInt <- D.urlsPageIds site
        (S.map unURIReference $ processorOURLs rx) d
    D.uCrawlPage crwl (M.crawlPagePageId msg) (M.crawlPagePageV msg)
        (D.processorId proc) pageIdsInt d
    pageIds <- D.lCrawlPagePageId (D.crawlId crwl) (M.crawlPagePageId msg)
        (M.crawlPagePageV msg) (D.processorId proc) d
    _ <- M.txCrawlPageIds site crwl pageIds mCrwl
    _ <- M.txCrawlPageData crwl msg proc (processorOData rx) mStrm
    L.info l $
        decodeUtf8 (unCrawlHref $
            toRouteHref (D.siteURL site, D.crawlSiteV crwl)) <> " PROC " <>
        show (D.unProcessorId $ D.processorId proc) <> " " <>
        show (D.unSiteURL $ D.siteURL site) <>
        show (D.unPageURL $ D.pageURL page) <> " " <>
        show (HTTP.statusCode $ HTTP.responseStatus res)


genProcessorI :: M.CrawlPage -> D.Processor -> D.Crawl -> D.Site -> D.Page ->
    ProcessorI
genProcessorI msg proc crwl site page = ProcessorI meta header body
    where
        url = URIAbsolute $ D.unSiteURL $
            D.pageURLAbs (D.siteURL site) (D.pageURL page)
        method = decodeUtf8 <$>
            M.crawlPageRequestMethod $ M.crawlPageRequest msg
        status = case M.crawlPageResponse msg of
            Right r -> Just $ toInteger $ M.crawlPageResponseStatus r
            _       -> empty
        duration = case M.crawlPageResponse msg of
            Right r -> Just $ round (M.crawlPageResponseDuration r * 1000)
            _       -> empty
        err = case M.crawlPageResponse msg of
            Left e  -> Just $ show e
            _       -> empty
        config = D.crawlProcessorConfig crwl ^? key (D.processorTag proc)
        meta = ProcessorIMeta {
            processorIMetaConfig   = config,
            processorIMetaDuration = duration,
            processorIMetaError    = err,
            processorIMetaMethod   = method,
            processorIMetaStatus   = status,
            processorIMetaURL      = url}
        header = case M.crawlPageBlob msg of
            Just b  -> M.crawlPageBlobHeader b
            Nothing -> M.empty
        body = case M.crawlPageBlob msg of
            Just b  -> toStrict $ M.crawlPageBlobBody b
            Nothing -> ""
