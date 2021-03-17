module ISX.CE.DB.URLPage (
    cEntryURLs,
    pageURLAbs,
    ) where


import           Control.Exception    (handle)
import           ISX.CE.DB.Query
import           ISX.CE.DB.Types
import           Network.URI
import           TPX.Com.URI
import qualified Data.Set             as S
import qualified Network.HTTP.Conduit as HTTP
import qualified TPX.Com.SQLite.Conn  as D


cEntryURLs :: MonadIO m => Site -> Crwl -> D.Conn -> m [()]
cEntryURLs s c d = do
    pIds <- urlsPageIds s (S.fromList urls) d
    cCrwlPagesEntry (crwlId c) (crwlPlugProcIds c) pIds d
    where
        urls = unSiteURL . pageURLAbs (siteURL s) . PageURL <$>
            catMaybes [parseURIReference "/"]

pageURLAbs :: SiteURL -> PageURL -> SiteURL
pageURLAbs s p = SiteURL $ relativeTo (unPageURL p) (unSiteURL s)


decomposeURL :: (MonadFail m, MonadIO m) => URI -> D.Conn -> m (Maybe Page)
decomposeURL url d = do
    req <- liftIO $ HTTP.parseRequest $ show url
    p <- storeReq req d
    return $ Just p

decomposeURLInt :: (MonadFail m, MonadIO m) => Site -> URI -> D.Conn ->
    m (Maybe Page)
decomposeURLInt s url d = do
    req <- liftIO $ HTTP.parseRequest $ show url
    if reqURISite req == unSiteURL (siteURL s)
        then decomposeURL url d
        else return Nothing

handleInvalidURL :: MonadIO m => HTTP.HttpException -> m (Maybe a)
handleInvalidURL ex = case ex of
    HTTP.InvalidUrlException _ _ -> return Nothing
    _                            -> bug ex

storeReq :: (MonadFail m, MonadIO m) => HTTP.Request -> D.Conn -> m Page
storeReq req d = do
    Just s <- rSiteIns (SiteURL sURL) d
    Just p <- rPageIns (s, PageURL pURL) d
    return p
    where
        sURL = reqURISite req
        pURL = reqURIPage req

urlsPageIds :: MonadIO m => Site -> Set URI -> D.Conn -> m [PageId]
urlsPageIds s urls d = do
    urls' <- mapM c2 $ toList urls
    let urls'' = catMaybes urls'
    return [pageId p | p <- urls'', pageSiteId p == siteId s]
    where
        c2 :: MonadIO m => URI -> m (Maybe Page)
        c2 u = do
            u' <- liftIO $ handle handleInvalidURL $ decomposeURLInt s u d
            case u' of
                Just u'' -> return $ Just u''
                Nothing  -> return Nothing
