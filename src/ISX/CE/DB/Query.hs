{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}


module ISX.CE.DB.Query (
    cCrwl,
    cCrwlPagesEntry,
    cPlugProc,
    cPlugStrm,
    cSite,
    --
    lCrwl,
    lCrwlPagePageIdEntry,
    lPlugProc,
    lPlugStrm,
    --
    rCrwl,
    rPageId,
    rPageIns,
    rPlugProc,
    rPlugStrm,
    rSite,
    rSiteId,
    rSiteIns,
    --
    --
    dPlugProc,
    dPlugStrm,
    ) where


import           Data.Time.Clock
import           ISX.CE.DB.Types
import           Network.URI
import           TPX.Com.Cursor
import           TPX.Com.SQLite.Query
import           TPX.Com.UUID
import qualified Crypto.Hash          as Hash
import qualified Data.Aeson           as A
import qualified TPX.Com.SQLite.Conn  as D
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
cCrwl :: MonadIO m => SiteId -> A.Value -> [PlugProcId] -> [PlugStrmId] ->
    D.Conn -> m (Maybe SiteV)
cCrwl sId j ppIds psIds d = withTransaction d $ do
    t <- liftIO getCurrentTime
    let sV_ = Just $ SiteV t
    let p0 = (sId, sV_, A.encode j)
    executeW q0 p0 d
    let p1 = map (sId, sV_,) ppIds
    executeManyW q1 p1 d
    let p2 = map (sId, sV_,) psIds
    executeManyW q2 p2 d
    return sV_
    where
        q0 = " \
        \   /* cCrwl.0 */ \
        \   INSERT INTO crwl ( \
        \       site_id, \
        \       site_v, \
        \       plug_proc_conf \
        \   ) \
        \   VALUES (?, ?, ?) \
        \ "
        q1 = " \
        \   /* cCrwl.1 */ \
        \   INSERT INTO crwl_plug_proc ( \
        \       site_id, \
        \       site_v, \
        \       plug_proc_id \
        \   ) \
        \   VALUES (?, ?, ?) \
        \   ON CONFLICT DO NOTHING \
        \ "
        q2 = " \
        \   /* cCrwl.2 */ \
        \   INSERT INTO crwl_plug_strm ( \
        \       site_id, \
        \       site_v, \
        \       plug_strm_id \
        \   ) \
        \   VALUES (?, ?, ?) \
        \   ON CONFLICT DO NOTHING \
        \ "

cCrwlPagesEntry :: MonadIO m => CrwlId -> [PlugProcId] -> [PageId] -> D.Conn ->
    m [()]
cCrwlPagesEntry (sId, sV) ppIds pIds d = forM ppIds $ \ppId ->
    executeManyW q (p ppId) d
    where
        q = " \
        \   /* cCrwlPagesEntry */ \
        \   INSERT INTO crwl_page ( \
        \       site_id, \
        \       site_v, \
        \       plug_proc_id, \
        \       page_id \
        \   ) \
        \   VALUES (?, ?, ?, ?) \
        \   ON CONFLICT DO NOTHING \
        \ "
        p ppId = map (sId, sV, ppId,) pIds

cPlugProc :: MonadIO m => URI -> Text -> D.Conn -> m (Maybe PlugProcId)
cPlugProc url tag d = do
    ppId <- genUUID
    let p = (ppId, url, tag)
    executeW q p d
    r <- rPlugProcURL url d
    return $ plugProcId <$> r
    where
        q = " \
        \   /* cPlugProc */ \
        \   INSERT INTO plug_proc ( \
        \       plug_proc_id, \
        \       url, \
        \       tag \
        \   ) \
        \   VALUES (?, ?, ?) \
        \   ON CONFLICT (url) DO UPDATE SET \
        \       tag = tag \
        \ "

cPlugStrm :: MonadIO m => URI -> Text -> D.Conn -> m (Maybe PlugStrmId)
cPlugStrm url tag d = do
    psId <- genUUID
    let p = (psId, url, tag)
    executeW q p d
    r <- rPlugStrmURL url d
    return $ plugStrmId <$> r
    where
        q = " \
        \   /* cPlugStrm */ \
        \   INSERT INTO plug_strm ( \
        \       plug_strm_id, \
        \       url, \
        \       tag \
        \   ) \
        \   VALUES (?, ?, ?) \
        \   ON CONFLICT (url) DO UPDATE SET \
        \       tag = tag \
        \ "

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
lCrwl :: MonadIO m => SiteId -> Cursor -> D.Conn -> m [Crwl]
lCrwl sId = curse (lCrwlF sId) (lCrwlN sId) (lCrwlP sId)

lCrwlPagePageIdEntry :: MonadIO m => CrwlId -> D.Conn -> m [PageId]
lCrwlPagePageIdEntry cId = queryR q p
    where
        q = " \
        \   /* lCrwlPagePageIdEntry */ \
        \   SELECT DISTINCT \
        \       page_id \
        \   FROM \
        \       crwl_page \
        \   WHERE \
        \       (site_id, site_v) = (?, ?) \
        \       AND \
        \       p_page_id IS NULL \
        \       AND \
        \       p_page_v IS NULL \
        \       AND \
        \       page_v IS NULL \
        \ "
        p = cId

lPlugProc :: MonadIO m => Cursor -> D.Conn -> m [PlugProc]
lPlugProc = curse lPlugProcF lPlugProcN lPlugProcP

lPlugStrm :: MonadIO m => Cursor -> D.Conn -> m [PlugStrm]
lPlugStrm = curse lPlugStrmF lPlugStrmN lPlugStrmP
--------------------------------------------------------------------------------
rCrwl :: MonadIO m => CrwlId -> D.Conn -> m (Maybe Crwl)
rCrwl cId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rCrwl */ \
        \   SELECT * FROM d_crwl_0 \
        \   WHERE \
        \       (site_id, site_v) = (?, ?) \
        \   LIMIT 1 \
        \ "
        p = cId

rPageId :: MonadIO m => (SiteId, PageId) -> D.Conn -> m (Maybe Page)
rPageId (sId, pId) d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rPageId */ \
        \   SELECT * FROM d_page_0 \
        \   WHERE \
        \       (site_id, page_id) = (?, ?) \
        \   LIMIT 1 \
        \ "
        p = (sId, pId)

rPageIns :: MonadIO m => (Site, PageURL) -> D.Conn -> m (Maybe Page)
rPageIns (s, pURL) d = rPage (siteId s, pURL) d >>= \case
    Just p  -> return $ Just p
    Nothing -> cPageAuto (s, unPageURL pURL) d >> rPage (siteId s, pURL) d

rPlugProc :: MonadIO m => PlugProcId -> D.Conn -> m (Maybe PlugProc)
rPlugProc ppId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rPlugProc */ \
        \   SELECT * FROM d_plug_proc_0 \
        \   WHERE \
        \       plug_proc_id = ? \
        \   LIMIT 1 \
        \ "
        p = [ppId]

rPlugStrm :: MonadIO m => PlugStrmId -> D.Conn -> m (Maybe PlugStrm)
rPlugStrm psId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rPlugStrm */ \
        \   SELECT * FROM d_plug_strm_0 \
        \   WHERE \
        \       plug_strm_id = ? \
        \   LIMIT 1 \
        \ "
        p = [psId]

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

rSiteIns :: MonadIO m => SiteURL -> D.Conn -> m (Maybe Site)
rSiteIns sURL d = rSite sURL d >>= \case
    Just s  -> return $ Just s
    Nothing -> cSiteAuto (unSiteURL sURL) d >> rSite sURL d
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
dPlugProc :: MonadIO m => PlugProcId -> D.Conn -> m ()
dPlugProc ppId = executeW q p
    where
        q = " \
        \   /* dPlugProc */ \
        \   DELETE FROM plug_proc \
        \   WHERE \
        \       plug_proc_id = ? \
        \ "
        p = [ppId]

dPlugStrm :: MonadIO m => PlugStrmId -> D.Conn -> m ()
dPlugStrm psId = executeW q p
    where
        q = " \
        \   /* dPlugStrm */ \
        \   DELETE FROM plug_strm \
        \   WHERE \
        \       plug_strm_id = ? \
        \ "
        p = [psId]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
lCrwlF :: MonadIO m => SiteId -> Integer -> D.Conn -> m [Crwl]
lCrwlF sId curLim d = do
    rs <- queryR q p d :: MonadIO m => m [CrwlId']
    catMaybes <$> forM rs (\r -> rCrwl (crwlSiteId' r, crwlSiteV' r) d)
    where
        q = " \
        \   /* lCrwlF */ \
        \   SELECT site_id, site_v FROM crwl \
        \   WHERE \
        \       site_id = ? \
        \   ORDER BY \
        \       site_v DESC \
        \   LIMIT ? \
        \ "
        p = (sId, curLim)

lCrwlN :: MonadIO m => SiteId -> Integer -> ByteString -> D.Conn -> m [Crwl]
lCrwlN sId curLim curPosN d = do
    rs <- queryR q p d :: MonadIO m => m [CrwlId']
    catMaybes <$> forM rs (\r -> rCrwl (crwlSiteId' r, crwlSiteV' r) d)
    where
        q = " \
        \   /* lCrwlN */ \
        \   SELECT site_id, site_v FROM crwl \
        \   WHERE \
        \       site_id = ? \
        \       AND \
        \       site_v < ? \
        \   ORDER BY \
        \       site_v DESC \
        \   LIMIT ? \
        \ "
        p = (sId, curPosN, curLim)

lCrwlP :: MonadIO m => SiteId -> Integer -> ByteString -> D.Conn -> m [Crwl]
lCrwlP sId curLim curPosP d = do
    rs <- queryR q p d :: MonadIO m => m [CrwlId']
    reverse . catMaybes <$> forM rs (\r -> rCrwl (crwlSiteId' r, crwlSiteV' r) d)
    where
        q = " \
        \   /* lCrwlP */ \
        \   SELECT site_id, site_v FROM crwl \
        \   WHERE \
        \       site_id = ? \
        \       AND \
        \       site_v > ? \
        \   ORDER BY \
        \       site_v ASC \
        \   LIMIT ? \
        \ "
        p = (sId, curPosP, curLim)
--------------------------------------------------------------------------------
lPlugProcF :: MonadIO m => Integer -> D.Conn -> m [PlugProc]
lPlugProcF curLim d = do
    ppIds <- queryR q p d
    catMaybes <$> forM ppIds (`rPlugProc` d)
    where
        q = " \
        \   /* lPlugProcF */ \
        \   SELECT plug_proc_id FROM plug_proc \
        \   ORDER BY \
        \       t_ins DESC, \
        \       plug_proc_id DESC \
        \   LIMIT ? \
        \ "
        p = [curLim]

lPlugProcN :: MonadIO m => Integer -> ByteString -> D.Conn -> m [PlugProc]
lPlugProcN curLim curPosN d = do
    ppIds <- queryR q p d
    catMaybes <$> forM ppIds (`rPlugProc` d)
    where
        q = " \
        \   /* lPlugProcN */ \
        \   SELECT plug_proc_id FROM plug_proc \
        \   WHERE \
        \       t_ins < ? \
        \   ORDER BY \
        \       t_ins DESC, \
        \       plug_proc_id DESC \
        \   LIMIT ? \
        \ "
        p = (curPosN, curLim)

lPlugProcP :: MonadIO m => Integer -> ByteString -> D.Conn -> m [PlugProc]
lPlugProcP curLim curPosP d = do
    ppIds <- queryR q p d
    reverse . catMaybes <$> forM ppIds (`rPlugProc` d)
    where
        q = " \
        \   /* lPlugProcP */ \
        \   SELECT plug_proc_id FROM plug_proc \
        \   WHERE \
        \       t_ins > ? \
        \   ORDER BY \
        \       t_ins ASC, \
        \       plug_proc_id ASC \
        \   LIMIT ? \
        \ "
        p = (curPosP, curLim)
--------------------------------------------------------------------------------
lPlugStrmF :: MonadIO m => Integer -> D.Conn -> m [PlugStrm]
lPlugStrmF curLim d = do
    psIds <- queryR q p d
    catMaybes <$> forM psIds (`rPlugStrm` d)
    where
        q = " \
        \   /* lPlugStrmF */ \
        \   SELECT plug_strm_id FROM plug_strm \
        \   ORDER BY \
        \       t_ins DESC, \
        \       plug_strm_id DESC \
        \   LIMIT ? \
        \ "
        p = [curLim]

lPlugStrmN :: MonadIO m => Integer -> ByteString -> D.Conn -> m [PlugStrm]
lPlugStrmN curLim curPosN d = do
    psIds <- queryR q p d
    catMaybes <$> forM psIds (`rPlugStrm` d)
    where
        q = " \
        \   /* lPlugStrmN */ \
        \   SELECT plug_strm_id FROM plug_strm \
        \   WHERE \
        \       t_ins < ? \
        \   ORDER BY \
        \       t_ins DESC, \
        \       plug_strm_id DESC \
        \   LIMIT ? \
        \ "
        p = (curPosN, curLim)

lPlugStrmP :: MonadIO m => Integer -> ByteString -> D.Conn -> m [PlugStrm]
lPlugStrmP curLim curPosP d = do
    psIds <- queryR q p d
    reverse . catMaybes <$> forM psIds (`rPlugStrm` d)
    where
        q = " \
        \   /* lPlugStrmP */ \
        \   SELECT plug_strm_id FROM plug_strm \
        \   WHERE \
        \       t_ins > ? \
        \   ORDER BY \
        \       t_ins ASC, \
        \       plug_strm_id ASC \
        \   LIMIT ? \
        \ "
        p = (curPosP, curLim)
--------------------------------------------------------------------------------
cPageAuto :: MonadIO m => (Site, URI) -> D.Conn -> m (Maybe PageURL)
cPageAuto (s, url) d = do
    executeW q p d
    return $ Just $ PageURL url
    where
        q = " \
        \   /* cPageAuto */ \
        \   INSERT INTO page ( \
        \       page_id, \
        \       site_id, \
        \       url \
        \   ) \
        \   VALUES (?, ?, ?) \
        \   ON CONFLICT DO NOTHING \
        \ "
        url' = toText $ show (siteURL s) ++ show url
        pId = PageId $ hash url'
        p = (pId, siteId s, url)

cSiteAuto :: MonadIO m => URI -> D.Conn -> m (Maybe SiteURL)
cSiteAuto url d = do
    executeW q p d
    return $ Just $ SiteURL url
    where
        q = " \
        \   /* cSiteAuto */ \
        \   INSERT INTO isx.site ( \
        \       site_id, \
        \       url, \
        \       auto \
        \   ) \
        \   VALUES (?, ?, true) \
        \   ON CONFLICT (site_id) DO UPDATE SET \
        \       auto = true \
        \ "
        sId = SiteId $ hash $ show url
        p = (sId, url)

rPage :: MonadIO m => (SiteId, PageURL) -> D.Conn -> m (Maybe Page)
rPage (sId, pURL) d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rPage */ \
        \   SELECT * FROM d_page_0 \
        \   WHERE \
        \       (site_id, url) = (?, ?) \
        \   LIMIT 1 \
        \ "
        p = (sId, pURL)

rPlugProcURL :: MonadIO m => URI -> D.Conn -> m (Maybe PlugProc)
rPlugProcURL url d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rPlugProcURL */ \
        \   SELECT * FROM d_plug_proc_0 \
        \   WHERE \
        \       url = ? \
        \   LIMIT 1 \
        \ "
        p = [url]

rPlugStrmURL :: MonadIO m => URI -> D.Conn -> m (Maybe PlugStrm)
rPlugStrmURL url d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rPlugStrmURL */ \
        \   SELECT * FROM d_plug_strm_0 \
        \   WHERE \
        \       url = ? \
        \   LIMIT 1 \
        \ "
        p = [url]

hash :: Text -> Hash.Digest Hash.SHA256
hash t = Hash.hash (encodeUtf8 t :: ByteString)
