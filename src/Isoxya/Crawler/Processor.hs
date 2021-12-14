module Isoxya.Crawler.Processor (process) where


import           Control.Concurrent              (threadDelay)
import           Control.Exception               (handle)
import           Data.Time.Clock
import           Isoxya.API.Href
import           TiredPixel.Common.Snap.CoreUtil
import qualified Isoxya.DB                       as D
import qualified Isoxya.Msg                      as M
import qualified Network.HTTP.Conduit            as HTTP
import qualified Text.Regex                      as R
import qualified TiredPixel.Common.Log           as L
import qualified TiredPixel.Common.Net           as N


process :: L.Logger -> Text -> M.ChanProcessor -> N.Conn -> D.Conn ->
    M.MsgCrawler -> IO ()
process l ver mProc n d (stId, msg) = do
    Just st <- D.rSiteId stId d
    L.debug l $ show st
    Just crl <- D.rCrawl (M.crawlPageIdSiteId msg, M.crawlPageIdSiteV msg) d
    L.debug l $ show crl
    Just pg <- D.rPageId (D.crawlSiteId crl, M.crawlPageIdPageId msg) d
    let reqURL = D.pageURLAbs (D.siteURL st) (D.pageURL pg)
    let ua = userAgent ver
    let req = N.userAgentReq ua $ N.makeReq "GET" (D.unSiteURL reqURL) ""
    L.debug l $ show req
    tR <- getCurrentTime
    crlPg <- handle (httpH l (D.crawlId crl) (D.pageId pg) req tR) $ do
        res <- N.makeResLim reqLim req n
        L.debug l $ show res
        M.genCrawlPageResponse (D.crawlId crl) (D.pageId pg) req res tR <$>
            getCurrentTime
    L.debug l $ show crlPg
    _ <- M.txCrawlPage crl crlPg mProc
    L.info l $
        decodeUtf8 (unCrawlHref $
            toRouteHref (D.siteURL st, D.crawlSiteV crl)) <> " CRWL " <>
        decodeUtf8 (M.crawlPageRequestMethod $
            M.crawlPageRequest crlPg) <> " " <>
        show (D.unSiteURL $ D.siteURL st) <>
        show (D.unPageURL $ D.pageURL pg) <> " " <>
        M.showCrawlPageResponse (M.crawlPageResponse crlPg)
    limitRate l
    where
        reqLim = 1048576 -- 1 MB


httpH :: L.Logger -> D.CrawlId -> D.PageId -> HTTP.Request -> UTCTime ->
    HTTP.HttpException -> IO M.CrawlPage
httpH l crlId pgId req tR ex = do
    L.err l $ show ex
    return $ M.genCrawlPageError crlId pgId req (M.httpExResponseError ex) tR

limitRate :: L.Logger -> IO ()
limitRate l = do
    L.debug l $ "LIMITING " <> show rateLim <> " μs"
    threadDelay rateLim
    where
        rateLim = 1000000 -- 1 s

userAgent :: Text -> ByteString
userAgent ver = encodeUtf8 $ toText $ R.subRegex r str (toString ver)
    where
        str = "Isoxya/${VERSION} (+https://www.isoxya.com/)"
        r = R.mkRegex "\\$\\{VERSION\\}"
