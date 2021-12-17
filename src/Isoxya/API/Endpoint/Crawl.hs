module Isoxya.API.Endpoint.Crawl (
    createWithSite,
    listWithSite,
    readWithSite,
    ) where


import           Isoxya.API.Core
import qualified Isoxya.DB       as D
import qualified Isoxya.Msg      as M


createWithSite :: Handler b API ()
createWithSite = do
    m <- gets _msgCrwl
    d <- gets _db
    Just (st, stId) <- run notFound $ fCrawls d
    req_ <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just pros <- run notFound $ mapM (\proH ->
        fProcessorHref (Just proH) d) (crawlCProcessorHrefs req)
    Just strs <- run notFound $ mapM (\strH ->
        fStreamerHref (Just strH) d) (crawlCStreamerHrefs req)
    Just stV <- D.cCrawl stId (crawlCProcessorConfig req)
        (snd <$> pros) (snd <$> strs) d
    Just crl <- D.rCrawl (stId, stV) d
    _ <- D.cEntryURLs st crl d
    pgIds <- D.lCrawlPagePageIdEntry (stId, stV) d
    M.txCrawlPageIds st (stId, stV) pgIds m
    let r = genCrawl st crl
    created (unCrawlHref $ crawlHref r) r

listWithSite :: Handler b API ()
listWithSite = do
    d <- gets _db
    Just (st, stId) <- run notFound $ fCrawls d
    cur <- parseReq
    crls <- D.lCrawl stId cur d
    setResLink (unCrawlsHref (toRouteHref (D.siteURL st) :: CrawlsHref))
        (formatTime . D.unSiteV . D.crawlSiteV) crls
    rs <- forM crls $ \crl -> do
        Just st' <- D.rSiteId (D.crawlSiteId crl) d
        return $ genCrawl st' crl
    writeJSON rs

readWithSite :: Handler b API ()
readWithSite = do
    d <- gets _db
    Just ((st, crl), _) <- run notFound $ fCrawl d
    writeJSON $ genCrawl st crl
