module ISX.CE.DB.Query (
    cSite,
--    --
--    --
    rSite,
    rSiteId,
--    --
--    --
    ) where


import           ISX.CE.DB.Types
import           Network.URI
import           TPX.Com.SQLite.Query
import qualified Crypto.Hash          as Hash
import qualified TPX.Com.SQLite.Conn  as D
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
cSite :: MonadIO m => URI -> D.Conn -> m (Maybe SiteURL)
cSite url d = do
    executeW q p d
    r <- rSiteId sId d
    return $ siteURL <$> r
    where
        sId = SiteId $ hash $ show url
        q = " \
        \   /* cSite */ \
        \   INSERT INTO site ( \
        \       site_id, \
        \       url \
        \   ) \
        \   VALUES (?, ?) \
        \   ON CONFLICT (site_id) DO UPDATE SET \
        \       auto = false \
        \ "
        p = (sId, url)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
rSite :: MonadIO m => SiteURL -> D.Conn -> m (Maybe Site)
rSite sURL d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rSite */ \
        \   SELECT * FROM d_site_0 \
        \   WHERE \
        \       url = ? \
        \   LIMIT 1 \
        \ "
        p = [sURL]

rSiteId :: MonadIO m => SiteId -> D.Conn -> m (Maybe Site)
rSiteId sId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rSiteId */ \
        \   SELECT * FROM d_site_0 \
        \   WHERE \
        \       site_id = ? \
        \   LIMIT 1 \
        \ "
        p = [sId]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
hash :: Text -> Hash.Digest Hash.SHA256
hash t = Hash.hash (encodeUtf8 t :: ByteString)
