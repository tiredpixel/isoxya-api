module Isoxya.Msg.Query (
    genCrawlPageError,
    genCrawlPageResponse,
    --
    txCrawlPage,
    txCrawlPageData,
    txCrawlPageIds,
    --
    rx,
    ) where


import           Control.Concurrent.Chan
import           Data.Time.Clock
import           Isoxya.Msg.Type
import qualified Data.Aeson                as A
import qualified Data.CaseInsensitive      as CI
import qualified Data.Map                  as M
import qualified Isoxya.DB                 as D
import qualified Network.HTTP.Conduit      as HTTP
import qualified Network.HTTP.Types.Status as HTTP
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
genCrawlPageError ::
    D.Crawl -> D.PageId -> HTTP.Request -> CrawlPageResponseError ->
    UTCTime -> CrawlPage
genCrawlPageError crl pgId req resErr t = CrawlPage
        (D.crawlSiteId crl) (D.crawlSiteV crl)
        pgId (D.PageV t) req' (Left resErr) Nothing
    where
        req' = CrawlPageRequest (HTTP.method req) (HTTP.requestVersion req)

genCrawlPageResponse ::
    D.Crawl -> D.PageId -> HTTP.Request -> HTTP.Response LByteString ->
    UTCTime -> UTCTime -> CrawlPage
genCrawlPageResponse crl pgId req res t t' = CrawlPage
        (D.crawlSiteId crl) (D.crawlSiteV crl)
        pgId (D.PageV t) req' (Right res') (Just blb)
    where
        req' = CrawlPageRequest (HTTP.method req) (HTTP.requestVersion req)
        res' = CrawlPageResponse (HTTP.statusCode $ HTTP.responseStatus res)
            (HTTP.responseVersion res)
            (toRational $ nominalDiffTimeToSeconds $ diffUTCTime t' t)
        blb = CrawlPageBlob
            (M.fromList (hConvert <$> HTTP.responseHeaders res))
            (HTTP.responseBody res)
--------------------------------------------------------------------------------
txCrawlPage :: MonadIO m =>
    D.Crawl -> CrawlPage -> ChanProcessor -> m ()
txCrawlPage crl crlPg m = liftIO $ writeList2Chan m msgs
    where
        msgs = [(proId, crlPg) | proId <- D.crawlProcessorIds crl]

txCrawlPageData :: MonadIO m =>
    D.Crawl -> CrawlPage -> D.Processor -> A.Value -> ChanStreamer -> m ()
txCrawlPageData crl crlPg pro dat m = liftIO $ writeList2Chan m msgs
    where
        crlPgData = genCrawlPageData crl crlPg pro dat
        msgs = [(strmId, crlPgData) | strmId <- D.crawlStreamerIds crl]

txCrawlPageIds :: MonadIO m =>
    D.Site -> D.Crawl -> [D.PageId] -> ChanCrawler -> m ()
txCrawlPageIds site crl pgIds m = liftIO $ writeList2Chan m msgs
    where
        msgs = [(D.siteId site, genCrawlPageId crl pgId) | pgId <- pgIds]
--------------------------------------------------------------------------------
rx ::
    Chan a -> (a -> IO ()) -> IO ()
rx m f = getChanContents m >>= mapM_ f
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
genCrawlPageId ::
    D.Crawl -> D.PageId -> CrawlPageId
genCrawlPageId crl = CrawlPageId (D.crawlSiteId crl) (D.crawlSiteV crl)

genCrawlPageData ::
    D.Crawl -> CrawlPage -> D.Processor -> A.Value -> CrawlPageData
genCrawlPageData crl crlPg pro = CrawlPageData
    (D.crawlSiteId crl) (D.crawlSiteV crl)
    (crawlPagePageId crlPg) (crawlPagePageV crlPg) (D.processorId pro)

hConvert ::
    (CI.CI ByteString, ByteString) -> (Text, Text)
hConvert (k, v) = (decodeUtf8 $ CI.original k, decodeUtf8 v)
