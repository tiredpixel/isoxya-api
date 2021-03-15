module ISX.CE.DB.Query (
    cPlugProc,
    cPlugStrm,
    cSite,
    --
    lPlugProc,
    lPlugStrm,
    --
    rPlugProc,
    rPlugStrm,
    rSite,
    rSiteId,
    --
    --
    dPlugProc,
    dPlugStrm,
    ) where


import           ISX.CE.DB.Types
import           Network.URI
import           TPX.Com.Cursor
import           TPX.Com.SQLite.Query
import           TPX.Com.UUID
import qualified Crypto.Hash          as Hash
import qualified TPX.Com.SQLite.Conn  as D
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
cPlugProc :: MonadIO m => URI -> Text -> D.Conn -> m (Maybe PlugProcId)
cPlugProc url tag d = do
    ppId <- generateUUID
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
    psId <- generateUUID
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
lPlugProc :: MonadIO m => Cursor -> D.Conn -> m [PlugProc]
lPlugProc = curse lPlugProcF lPlugProcN lPlugProcP

lPlugStrm :: MonadIO m => Cursor -> D.Conn -> m [PlugStrm]
lPlugStrm = curse lPlugStrmF lPlugStrmN lPlugStrmP
--------------------------------------------------------------------------------
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
