module Isoxya.Crawler.Processor (process) where


import           Control.Concurrent              (threadDelay)
import           Control.Exception               (try)
import           Data.Time.Clock
import           Data.Version                    (showVersion)
import           Isoxya.API.Href
import           Paths_isoxya_api                (version)
import           TiredPixel.Common.Logger
import           TiredPixel.Common.Snap.CoreUtil
import qualified Isoxya.DB                       as D
import qualified Isoxya.Msg                      as M
import qualified Text.Regex                      as R
import qualified TiredPixel.Common.Net           as N


process :: M.ChanProcessor -> N.Conn -> D.Conn -> M.MsgCrawler -> IO ()
process mPro n d (stId, msg) = runLogger $ do
    Just st <- D.rSiteId stId d
    logDebugN $ show st
    Just crl <- D.rCrawl (M.crawlPageIdSiteId msg, M.crawlPageIdSiteV msg) d
    logDebugN $ show crl
    Just pg <- D.rPageId (D.crawlSiteId crl, M.crawlPageIdPageId msg) d
    let reqURL = D.pageURLAbs (D.siteURL st) (D.pageURL pg)
    let req = N.userAgentReq (encodeUtf8 agentDef) $ N.makeReq "GET"
            (D.unSiteURL reqURL) ""
    logDebugN $ show req
    tR <- liftIO getCurrentTime
    res_ <- (liftIO . try) (N.makeResLim reqLim req n)
    crlPg <- case res_ of
        Left ex   -> do
            logErrorN $ show ex
            return $ M.genCrawlPageError (D.crawlId crl) (D.pageId pg) req
                (M.httpExResponseError ex) tR
        Right res -> do
            logDebugN $ show res
            t <- liftIO getCurrentTime
            return $ M.genCrawlPageResponse (D.crawlId crl) (D.pageId pg) req
                res tR t
    logDebugN $ show crlPg
    _ <- M.txCrawlPage crl crlPg mPro
    logInfoN $
        decodeUtf8 (unCrawlHref $
            toRouteHref (D.siteURL st, D.crawlSiteV crl)) <> " CRL " <>
        decodeUtf8 (M.crawlPageRequestMethod $
            M.crawlPageRequest crlPg) <> " " <>
        show (D.unSiteURL $ D.siteURL st) <>
        show (D.unPageURL $ D.pageURL pg) <> " " <>
        M.showCrawlPageResponse (M.crawlPageResponse crlPg)
    limitRate
    where
        reqLim = 1048576 -- 1 MB


limitRate :: LoggingT IO ()
limitRate = do
    logDebugN $ "LIMITING " <> show rateLim <> " μs"
    liftIO $ threadDelay rateLim
    where
        rateLim = 1000000 -- 1 s

agentDef :: Text
agentDef = toText $ R.subRegex r str ver
    where
        ver = showVersion version
        str = "Isoxya/${VERSION} (+https://www.isoxya.com/)"
        r = R.mkRegex "\\$\\{VERSION\\}"
