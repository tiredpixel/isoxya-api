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
genCrawlPageError :: D.CrawlId -> D.PageId -> HTTP.Request ->
    CrawlPageResponseError -> UTCTime -> CrawlPage
genCrawlPageError (stId, stV) pgId req resErr tR = CrawlPage
        stId stV pgId (D.PageV tR) req' (Left resErr) Nothing
    where
        req' = CrawlPageRequest (HTTP.method req) (HTTP.requestVersion req)

genCrawlPageResponse :: D.CrawlId -> D.PageId -> HTTP.Request ->
    HTTP.Response LByteString -> UTCTime -> UTCTime -> CrawlPage
genCrawlPageResponse (stId, stV) pgId req res tR tE = CrawlPage
        stId stV pgId (D.PageV tR) req' (Right res') (Just blb)
    where
        req' = CrawlPageRequest (HTTP.method req) (HTTP.requestVersion req)
        res' = CrawlPageResponse (HTTP.statusCode $ HTTP.responseStatus res)
            (HTTP.responseVersion res) (toRational $ diffUTCTime tE tR)
        blb = CrawlPageBlob
            (M.fromList (hConvert <$> HTTP.responseHeaders res))
            (HTTP.responseBody res)
--------------------------------------------------------------------------------
txCrawlPage :: MonadIO m => D.Crawl -> CrawlPage -> ChanProcessor -> m ()
txCrawlPage crl crlPg ch = liftIO $ writeList2Chan ch msgs
    where
        msgs = [(proId, crlPg) | proId <- D.crawlProcessorIds crl]

txCrawlPageData :: MonadIO m => D.Crawl -> CrawlPage -> D.Processor ->
    A.Value -> ChanStreamer -> m ()
txCrawlPageData crl crlPg pro dat ch = liftIO $ writeList2Chan ch msgs
    where
        crlPgDat = CrawlPageData (D.crawlSiteId crl) (D.crawlSiteV crl)
            (crawlPagePageId crlPg) (crawlPagePageV crlPg)
            (D.processorId pro) dat
        msgs = [(strId, crlPgDat) | strId <- D.crawlStreamerIds crl]

txCrawlPageIds :: MonadIO m => D.Site -> D.CrawlId -> [D.PageId] ->
    ChanCrawler -> m ()
txCrawlPageIds st (stId, stV) pgIds ch = liftIO $ writeList2Chan ch msgs
    where
        crlPgId = CrawlPageId stId stV
        msgs = [(D.siteId st, crlPgId pgId) | pgId <- pgIds]
--------------------------------------------------------------------------------
rx :: Chan a -> (a -> IO ()) -> IO ()
rx ch f = getChanContents ch >>= mapM_ f
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
hConvert :: (CI.CI ByteString, ByteString) -> (Text, Text)
hConvert (k, v) = (decodeUtf8 $ CI.original k, decodeUtf8 v)
