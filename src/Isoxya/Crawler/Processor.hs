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
process l ver mProc n d (siteId, msg) = do
    Just site <- D.rSiteId siteId d
    L.debug l $ show site
    Just crwl <- D.rCrawl (M.crawlPageIdSiteId msg, M.crawlPageIdSiteV msg) d
    L.debug l $ show crwl
    Just page <- D.rPageId (D.crawlSiteId crwl, M.crawlPageIdPageId msg) d
    let reqURL = D.pageURLAbs (D.siteURL site) (D.pageURL page)
    let req = N.userAgentReq (encodeUtf8 $ userAgentStr ver) $
            N.makeReq "GET" (D.unSiteURL reqURL) ""
    L.debug l $ show req
    t <- getCurrentTime
    crwlPage <- handle (httpH l crwl (D.pageId page) req t) $ do
        res <- N.makeResLim reqLim req n
        L.debug l $ show res
        M.genCrawlPageResponse crwl (D.pageId page) req res t <$> getCurrentTime
    L.debug l $ show crwlPage
    _ <- M.txCrawlPage crwl crwlPage mProc
    L.info l $
        decodeUtf8 (unCrawlHref $
            toRouteHref (D.siteURL site, D.crawlSiteV crwl)) <> " CRWL " <>
        decodeUtf8 (M.crawlPageRequestMethod $
            M.crawlPageRequest crwlPage) <> " " <>
        show (D.unSiteURL $ D.siteURL site) <>
        show (D.unPageURL $ D.pageURL page) <> " " <>
        M.showCrawlPageResponse (M.crawlPageResponse crwlPage)
    L.debug l $ "LIMITING " <> show rateLim <> " μs"
    threadDelay rateLim
    where
        rateLim = 1000000 -- 1 s
        reqLim  = 1048576 -- 1 MB


httpH :: L.Logger -> D.Crawl -> D.PageId -> HTTP.Request -> UTCTime ->
    HTTP.HttpException -> IO M.CrawlPage
httpH l crwl pageId req t ex = do
    L.err l $ show ex
    return $ M.genCrawlPageError crwl pageId req (M.httpExResponseError ex) t

userAgentStr :: Text -> Text
userAgentStr ver = toText $ R.subRegex r str (toString ver)
    where
        str = "Isoxya/${VERSION} (+https://www.isoxya.com/)"
        r = R.mkRegex "\\$\\{VERSION\\}"
