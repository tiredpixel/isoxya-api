module Isoxya.API.Find (
    fCrawl,
    fCrawls,
    fProcessor,
    fProcessorHref,
    fSite,
    fStreamer,
    fStreamerHref,
    ) where


import           Isoxya.API.Href
import           Snap.Core                       hiding (pass)
import           TiredPixel.Common.Snap.CoreUtil
import qualified Isoxya.DB                       as D


fCrawl :: MonadSnap m => D.Conn -> MaybeT m ((D.Site, D.Crawl), D.CrawlId)
fCrawl d = do
    (st, stId) <- fSite d
    Just stV_ <- lift $ getParam "site_v"
    Just stV <- return $ toRouteId stV_
    Just crl <- D.rCrawl (stId, stV) d
    return ((st, crl), (D.crawlSiteId crl, D.crawlSiteV crl))

fCrawls :: MonadSnap m => D.Conn -> MaybeT m (D.Site, D.SiteId)
fCrawls = fSite

fProcessor :: MonadSnap m => D.Conn -> MaybeT m (D.Processor, D.ProcessorId)
fProcessor d = do
    Just proId_ <- lift $ getParam "processor_id"
    Just proId <- return $ toRouteId proId_
    Just pro <- D.rProcessor proId d
    return (pro, D.processorId pro)

fProcessorHref :: MonadSnap m => Maybe ProcessorHref -> D.Conn ->
    MaybeT m (D.Processor, D.ProcessorId)
fProcessorHref proH d = do
    Just proId <- return $ fromRouteHref =<< proH
    Just pro <- D.rProcessor proId d
    return (pro, D.processorId pro)

fSite :: MonadSnap m => D.Conn -> MaybeT m (D.Site, D.SiteId)
fSite d = do
    Just stURL_ <- lift $ getParam "site_id"
    Just stURL <- return $ toRouteId stURL_
    Just st <- D.rSite stURL d
    return (st, D.siteId st)

fStreamer :: MonadSnap m => D.Conn -> MaybeT m (D.Streamer, D.StreamerId)
fStreamer d = do
    Just strId_ <- lift $ getParam "streamer_id"
    Just strId <- return $ toRouteId strId_
    Just str <- D.rStreamer strId d
    return (str, D.streamerId str)

fStreamerHref :: MonadSnap m => Maybe StreamerHref -> D.Conn ->
    MaybeT m (D.Streamer, D.StreamerId)
fStreamerHref strH d = do
    Just strId <- return $ fromRouteHref =<< strH
    Just str <- D.rStreamer strId d
    return (str, D.streamerId str)
