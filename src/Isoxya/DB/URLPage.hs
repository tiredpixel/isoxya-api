module Isoxya.DB.URLPage (
    cEntryURLs,
    pageURLAbs,
    urlsPageIds,
    ) where


import           Control.Exception             (handle)
import           Isoxya.DB.Query
import           Isoxya.DB.Type
import           Network.URI
import           TiredPixel.Common.URI
import qualified Data.Set                      as S
import qualified Network.HTTP.Conduit          as HTTP
import qualified TiredPixel.Common.SQLite.Conn as D


cEntryURLs :: MonadIO m => Site -> Crawl -> D.Conn -> m [()]
cEntryURLs st crl d = do
    pgIds <- urlsPageIds st (S.fromList urls) d
    cCrawlPagesEntry (crawlId crl) (crawlProcessorIds crl) pgIds d
    where
        urls = unSiteURL . pageURLAbs (siteURL st) . PageURL <$>
            catMaybes [parseURIReference "/"]

pageURLAbs :: SiteURL -> PageURL -> SiteURL
pageURLAbs stURL pgURL = SiteURL $
    relativeTo (unPageURL pgURL) (unSiteURL stURL)

urlsPageIds :: MonadIO m => Site -> Set URI -> D.Conn -> m [PageId]
urlsPageIds st urls d = do
    urls' <- mapM c2 $ toList urls
    let urls'' = catMaybes urls'
    return [pageId p | p <- urls'', pageSiteId p == siteId st]
    where
        c2 :: MonadIO m => URI -> m (Maybe Page)
        c2 u = do
            u' <- liftIO $ handle handleInvalidURL $
                decomposeURLInt st u d
            case u' of
                Just u'' -> return $ Just u''
                Nothing  -> return Nothing


decomposeURL :: (MonadFail m, MonadIO m) => URI -> D.Conn -> m (Maybe Page)
decomposeURL url d = do
    req <- liftIO $ HTTP.parseRequest $ show url
    pg <- storeReq req d
    return $ Just pg

decomposeURLInt :: (MonadFail m, MonadIO m) => Site -> URI -> D.Conn ->
    m (Maybe Page)
decomposeURLInt st url d = do
    req <- liftIO $ HTTP.parseRequest $ show url
    if reqURISite req == unSiteURL (siteURL st)
        then decomposeURL url d
        else return Nothing

handleInvalidURL :: MonadIO m => HTTP.HttpException -> m (Maybe a)
handleInvalidURL ex = case ex of
    HTTP.InvalidUrlException _ _ -> return Nothing
    _                            -> bug ex

storeReq :: (MonadFail m, MonadIO m) => HTTP.Request -> D.Conn -> m Page
storeReq req d = do
    Just st <- rSiteIns (SiteURL stURL) d
    Just pg <- rPageIns (st, PageURL pgURL) d
    return pg
    where
        stURL = reqURISite req
        pgURL = reqURIPage req
