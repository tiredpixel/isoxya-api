module Isoxya.API.Find (
    fCrawl,
    fCrawls,
    fProcessor,
    fProcessorHref,
    fProcessors,
    fSite,
    fSites,
    fStreamer,
    fStreamerHref,
    fStreamers,
    ) where


import           Isoxya.API.Auth
import           Isoxya.API.Href
import           Snap.Core                       hiding (pass)
import           TiredPixel.Common.Snap.CoreUtil
import qualified Isoxya.DB                       as D


fCrawl :: MonadSnap m => APerm -> D.Conn ->
    MaybeT m ((D.Site, D.Crawl), D.CrawlId)
fCrawl AR d = do
    (site, siteId) <- fSite AR d
    Just siteV_ <- lift $ getParam "site_v"
    Just siteV <- return $ toRouteId siteV_
    Just crwl <- D.rCrawl (siteId, siteV) d
    return ((site, crwl), (D.crawlSiteId crwl, D.crawlSiteV crwl))
fCrawl AW _ = fail "fCrawl/AW not allowed"

fCrawls :: MonadSnap m => APerm -> D.Conn -> MaybeT m (D.Site, D.SiteId)
fCrawls _ = fSite AR

fProcessor :: MonadSnap m => APerm -> D.Conn ->
    MaybeT m (D.Processor, D.ProcessorId)
fProcessor _ d = do
    Just procId_ <- lift $ getParam "processor_id"
    Just procId <- return $ toRouteId procId_
    Just proc <- D.rProcessor procId d
    return (proc, D.processorId proc)

fProcessorHref :: MonadSnap m => Maybe ProcessorHref -> APerm -> D.Conn ->
    MaybeT m (D.Processor, D.ProcessorId)
fProcessorHref procH _ d = do
    Just procId <- return $ fromRouteHref =<< procH
    Just proc <- D.rProcessor procId d
    return (proc, D.processorId proc)

fProcessors :: MonadSnap m => APerm -> D.Conn -> MaybeT m ()
fProcessors _ _ = pass

fSite :: MonadSnap m => APerm -> D.Conn -> MaybeT m (D.Site, D.SiteId)
fSite AR d = do
    Just siteURL_ <- lift $ getParam "site_id"
    Just siteURL <- return $ toRouteId siteURL_
    Just site <- D.rSite siteURL d
    return (site, D.siteId site)
fSite AW _ = fail "fSite/AW not allowed"

fSites :: MonadSnap m => APerm -> D.Conn -> MaybeT m ()
fSites AR _ = fail "fSites/AR not allowed"
fSites AW _ = pass

fStreamer :: MonadSnap m => APerm -> D.Conn ->
    MaybeT m (D.Streamer, D.StreamerId)
fStreamer _ d = do
    Just strmId_ <- lift $ getParam "streamer_id"
    Just strmId <- return $ toRouteId strmId_
    Just strm <- D.rStreamer strmId d
    return (strm, D.streamerId strm)

fStreamerHref :: MonadSnap m => Maybe StreamerHref -> APerm -> D.Conn ->
    MaybeT m (D.Streamer, D.StreamerId)
fStreamerHref strmH _ d = do
    Just strmId <- return $ fromRouteHref =<< strmH
    Just strm <- D.rStreamer strmId d
    return (strm, D.streamerId strm)

fStreamers :: MonadSnap m => APerm -> D.Conn -> MaybeT m ()
fStreamers _ _ = pass
