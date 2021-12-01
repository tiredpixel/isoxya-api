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
cEntryURLs site crwl d = do
    pageIds <- urlsPageIds site (S.fromList urls) d
    cCrawlPagesEntry (crawlId crwl) (crawlProcessorIds crwl) pageIds d
    where
        urls = unSiteURL . pageURLAbs (siteURL site) . PageURL <$>
            catMaybes [parseURIReference "/"]

pageURLAbs :: SiteURL -> PageURL -> SiteURL
pageURLAbs sURL pURL = SiteURL $ relativeTo (unPageURL pURL) (unSiteURL sURL)

urlsPageIds :: MonadIO m => Site -> Set URI -> D.Conn -> m [PageId]
urlsPageIds site urls d = do
    urls' <- mapM c2 $ toList urls
    let urls'' = catMaybes urls'
    return [pageId page | page <- urls'', pageSiteId page == siteId site]
    where
        c2 :: MonadIO m => URI -> m (Maybe Page)
        c2 u = do
            u' <- liftIO $ handle handleInvalidURL $
                decomposeURLInternal site u d
            case u' of
                Just u'' -> return $ Just u''
                Nothing  -> return Nothing


decomposeURL :: (MonadFail m, MonadIO m) => URI -> D.Conn -> m (Maybe Page)
decomposeURL url d = do
    req <- liftIO $ HTTP.parseRequest $ show url
    page <- storeRequest req d
    return $ Just page

decomposeURLInternal :: (MonadFail m, MonadIO m) => Site -> URI -> D.Conn ->
    m (Maybe Page)
decomposeURLInternal site url d = do
    req <- liftIO $ HTTP.parseRequest $ show url
    if reqURISite req == unSiteURL (siteURL site)
        then decomposeURL url d
        else return Nothing

handleInvalidURL :: MonadIO m => HTTP.HttpException -> m (Maybe a)
handleInvalidURL ex = case ex of
    HTTP.InvalidUrlException _ _ -> return Nothing
    _                            -> bug ex

storeRequest :: (MonadFail m, MonadIO m) => HTTP.Request -> D.Conn -> m Page
storeRequest req d = do
    Just site <- rSiteIns (SiteURL sURL) d
    Just page <- rPageIns (site, PageURL pURL) d
    return page
    where
        sURL = reqURISite req
        pURL = reqURIPage req
