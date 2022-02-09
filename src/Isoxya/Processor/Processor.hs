module Isoxya.Processor.Processor (process) where


import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Map                           as M
import qualified Data.Set                           as S
import           Isoxya.API.Href
import qualified Isoxya.DB                          as D
import qualified Isoxya.Msg                         as M
import qualified Network.HTTP.Conduit               as HTTP
import qualified Network.HTTP.Types                 as HTTP
import           TiredPixel.Common.Isoxya.Processor
import           TiredPixel.Common.Logger
import qualified TiredPixel.Common.Net              as N
import           TiredPixel.Common.Snap.CoreUtil
import           TiredPixel.Common.URI


process :: M.ChanStreamer -> M.ChanCrawler -> N.Conn -> D.Conn ->
    M.MsgProcessor -> IO ()
process mStr mCrl n d (proId, msg) = runLogger $ do
    Just pro <- D.rProcessor proId d
    logDebugN $ show pro
    Just crl <- D.rCrawl (M.crawlPageSiteId msg, M.crawlPageSiteV msg) d
    logDebugN $ show crl
    Just st <- D.rSiteId (D.crawlSiteId crl) d
    logDebugN $ show st
    Just pg <- D.rPageId (D.crawlSiteId crl, M.crawlPagePageId msg) d
    let tx = genProcessorI msg pro crl st pg
    logDebugN $ show tx
    let req = N.jsonReq $ N.makeReq' "POST"
            (D.unProcessorURL $ D.processorURL pro) (encode tx)
    logDebugN $ show req
    logDebugN $ decodeUtf8 $ encode tx
    res <- N.makeRes req n
    logDebugN $ show res
    logDebugN $ decodeUtf8 $ HTTP.responseBody res
    let Just rx = decode $ HTTP.responseBody res :: Maybe ProcessorO
    pgIdsInt <- D.urlsPageIds st
        (S.map unURIReference $ processorOURLs rx) d
    D.uCrawlPage crl (M.crawlPagePageId msg) (M.crawlPagePageV msg)
        (D.processorId pro) pgIdsInt d
    pgIds <- D.lCrawlPagePageId (D.crawlId crl) (M.crawlPagePageId msg)
        (M.crawlPagePageV msg) (D.processorId pro) d
    _ <- M.txCrawlPageIds st (D.crawlId crl) pgIds mCrl
    _ <- M.txCrawlPageData crl msg pro (processorOData rx) mStr
    logInfoN $
        decodeUtf8 (unCrawlHref $
            toRouteHref (D.siteURL st, D.crawlSiteV crl)) <> " PRO " <>
        show (D.unProcessorId $ D.processorId pro) <> " " <>
        show (D.unSiteURL $ D.siteURL st) <>
        show (D.unPageURL $ D.pageURL pg) <> " " <>
        show (HTTP.statusCode $ HTTP.responseStatus res)


genProcessorI :: M.CrawlPage -> D.Processor -> D.Crawl -> D.Site -> D.Page ->
    ProcessorI
genProcessorI msg pro crl st pg = ProcessorI meta header body
    where
        url = URIAbsolute $ D.unSiteURL $
            D.pageURLAbs (D.siteURL st) (D.pageURL pg)
        method = decodeUtf8 <$>
            M.crawlPageRequestMethod $ M.crawlPageRequest msg
        status = case M.crawlPageResponse msg of
            Right r -> Just $ toInteger $ M.crawlPageResponseStatus r
            _       -> empty
        duration = case M.crawlPageResponse msg of
            Right r -> Just $ fromRational $ M.crawlPageResponseDuration r
            _       -> empty
        err = case M.crawlPageResponse msg of
            Left e -> Just $ show e
            _      -> empty
        conf = D.crawlProcessorConfig crl ^? key (D.processorTag pro)
        meta = ProcessorIMeta {
            processorIMetaConfig   = conf,
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
