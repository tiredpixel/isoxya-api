module Isoxya.API.Endpoint.Crawl (
    createSite,
    listSite,
    readSite,
    ) where


import           Isoxya.API.Core
import qualified Isoxya.DB       as D
import qualified Isoxya.Msg      as M


createSite :: Handler b API ()
createSite = do
    m <- gets _msgCrwl
    d <- gets _db
    Just (site, siteId) <- run notFound $ fCrawls AW d
    req_ <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just procs <- run notFound $ mapM (\procH ->
        fProcessorHref (Just procH) AR d) (crawlCProcessorHrefs req)
    Just strms <- run notFound $ mapM (\strmH ->
        fStreamerHref (Just strmH) AR d) (crawlCStreamerHrefs req)
    Just siteV <- D.cCrawl siteId (crawlCProcessorConfig req)
        (snd <$> procs) (snd <$> strms) d
    Just crwl <- D.rCrawl (siteId, siteV) d
    _ <- D.cEntryURLs site crwl d
    pageIds <- D.lCrawlPagePageIdEntry (siteId, siteV) d
    M.txCrawlPageIds site crwl pageIds m
    let r = genCrawl site crwl
    created (unCrawlHref $ crawlHref r) r

listSite :: Handler b API ()
listSite = do
    d <- gets _db
    Just (site, siteId) <- run notFound $ fCrawls AR d
    cur <- parseReq
    crwls <- D.lCrawl siteId cur d
    setResLink (unCrawlsHref (toRouteHref (D.siteURL site) :: CrawlsHref))
        (formatTime . D.unSiteV . D.crawlSiteV) crwls
    rs <- forM crwls $ \crwl -> do
        Just site' <- D.rSiteId (D.crawlSiteId crwl) d
        return $ genCrawl site' crwl
    writeJSON rs

readSite :: Handler b API ()
readSite = do
    d <- gets _db
    Just ((site, crwl), _) <- run notFound $ fCrawl AR d
    writeJSON $ genCrawl site crwl
