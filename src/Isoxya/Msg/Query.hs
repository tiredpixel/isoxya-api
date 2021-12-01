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
genCrawlPageError :: D.Crawl -> D.PageId -> HTTP.Request ->
    CrawlPageResponseError -> UTCTime -> CrawlPage
genCrawlPageError crwl pageId req resErr t = CrawlPage
        (D.crawlSiteId crwl) (D.crawlSiteV crwl)
        pageId (D.PageV t) req' (Left resErr) Nothing
    where
        req' = CrawlPageRequest (HTTP.method req) (HTTP.requestVersion req)

genCrawlPageResponse :: D.Crawl -> D.PageId -> HTTP.Request ->
    HTTP.Response LByteString -> UTCTime -> UTCTime -> CrawlPage
genCrawlPageResponse crwl pageId req res t t' = CrawlPage
        (D.crawlSiteId crwl) (D.crawlSiteV crwl)
        pageId (D.PageV t) req' (Right res') (Just blob)
    where
        req' = CrawlPageRequest (HTTP.method req) (HTTP.requestVersion req)
        res' = CrawlPageResponse (HTTP.statusCode $ HTTP.responseStatus res)
            (HTTP.responseVersion res)
            (nominalDiffTimeToSeconds $ diffUTCTime t' t)
        blob = CrawlPageBlob
            (M.fromList (hConvert <$> HTTP.responseHeaders res))
            (HTTP.responseBody res)
--------------------------------------------------------------------------------
txCrawlPage :: MonadIO m => D.Crawl -> CrawlPage -> ChanProcessor -> m ()
txCrawlPage crwl crwlPage m = liftIO $ writeList2Chan m msgs
    where
        msgs = [(procId, crwlPage) | procId <- D.crawlProcessorIds crwl]

txCrawlPageData :: MonadIO m => D.Crawl -> CrawlPage -> D.Processor ->
    A.Value -> ChanStreamer -> m ()
txCrawlPageData crwl crwlPage proc dat m = liftIO $ writeList2Chan m msgs
    where
        crwlPageData = genCrawlPageData crwl crwlPage proc dat
        msgs = [(strmId, crwlPageData) | strmId <- D.crawlStreamerIds crwl]

txCrawlPageIds :: MonadIO m => D.Site -> D.Crawl -> [D.PageId] -> ChanCrawler ->
    m ()
txCrawlPageIds site crwl pageIds m = liftIO $ writeList2Chan m msgs
    where
        msgs = [(D.siteId site, genCrawlPageId crwl pageId) | pageId <- pageIds]
--------------------------------------------------------------------------------
rx :: Chan a -> (a -> IO ()) -> IO ()
rx m f = getChanContents m >>= mapM_ f
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
genCrawlPageId :: D.Crawl -> D.PageId -> CrawlPageId
genCrawlPageId crwl = CrawlPageId (D.crawlSiteId crwl) (D.crawlSiteV crwl)

genCrawlPageData :: D.Crawl -> CrawlPage -> D.Processor -> A.Value ->
    CrawlPageData
genCrawlPageData crwl crwlPage proc = CrawlPageData
    (D.crawlSiteId crwl) (D.crawlSiteV crwl)
    (crawlPagePageId crwlPage) (crawlPagePageV crwlPage) (D.processorId proc)

hConvert :: (CI.CI ByteString, ByteString) -> (Text, Text)
hConvert (k, v) = (decodeUtf8 $ CI.original k, decodeUtf8 v)
