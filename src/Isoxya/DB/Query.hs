{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}


module Isoxya.DB.Query (
    cCrawl,
    cCrawlPagesEntry,
    cProcessor,
    cSite,
    cStreamer,
    --
    lCrawl,
    lCrawlPagePageId,
    lCrawlPagePageIdEntry,
    lProcessor,
    lStreamer,
    --
    rCrawl,
    rPageId,
    rPageIns,
    rProcessor,
    rSite,
    rSiteId,
    rSiteIns,
    rStreamer,
    --
    uCrawlPage,
    --
    dProcessor,
    dStreamer,
    ) where


import           Data.Time.Clock
import           Isoxya.DB.Type
import           Network.URI
import           TiredPixel.Common.Cursor
import           TiredPixel.Common.SQLite.Query
import           TiredPixel.Common.UUID
import qualified Crypto.Hash                    as Hash
import qualified Data.Aeson                     as A
import qualified TiredPixel.Common.SQLite.Conn  as D
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
cCrawl :: MonadIO m => SiteId -> A.Value -> [ProcessorId] -> [StreamerId] ->
    D.Conn -> m (Maybe SiteV)
cCrawl sitId conf procIds strmIds d = withTransaction d $ do
    t <- liftIO getCurrentTime
    let sitV_ = Just $ SiteV t
    let p0 = (sitId, sitV_, A.encode conf)
    executeW q0 p0 d
    let p1 = map (sitId, sitV_,) procIds
    executeManyW q1 p1 d
    let p2 = map (sitId, sitV_,) strmIds
    executeManyW q2 p2 d
    return sitV_
    where
        q0 = " \
        \   /* cCrawl.0 */ \
        \   INSERT INTO crawl ( \
        \       site_id, \
        \       site_v, \
        \       processor_config \
        \   ) \
        \   VALUES (?, ?, ?) \
        \ "
        q1 = " \
        \   /* cCrawl.1 */ \
        \   INSERT INTO crawl_processor ( \
        \       site_id, \
        \       site_v, \
        \       processor_id \
        \   ) \
        \   VALUES (?, ?, ?) \
        \   ON CONFLICT DO NOTHING \
        \ "
        q2 = " \
        \   /* cCrawl.2 */ \
        \   INSERT INTO crawl_streamer ( \
        \       site_id, \
        \       site_v, \
        \       streamer_id \
        \   ) \
        \   VALUES (?, ?, ?) \
        \   ON CONFLICT DO NOTHING \
        \ "

cCrawlPagesEntry :: MonadIO m => CrawlId -> [ProcessorId] -> [PageId] ->
    D.Conn -> m [()]
cCrawlPagesEntry (sitId, sitV) procIds pagIds d = forM procIds $ \procId ->
    executeManyW q (p procId) d
    where
        q = " \
        \   /* cCrawlPagesEntry */ \
        \   INSERT INTO crawl_page ( \
        \       site_id, \
        \       site_v, \
        \       processor_id, \
        \       page_id \
        \   ) \
        \   VALUES (?, ?, ?, ?) \
        \   ON CONFLICT DO NOTHING \
        \ "
        p procId = map (sitId, sitV, procId,) pagIds

cProcessor :: MonadIO m => URI -> Text -> D.Conn -> m (Maybe ProcessorId)
cProcessor url tag d = do
    procId <- genUUID
    let p = (procId, url, tag)
    executeW q p d
    r <- rProcessorURL url d
    return $ processorId <$> r
    where
        q = " \
        \   /* cProcessor */ \
        \   INSERT INTO processor ( \
        \       processor_id, \
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
    r <- rSiteId sitId d
    return $ siteURL <$> r
    where
        sitId = SiteId $ hash $ show url
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
        p = (sitId, url)

cStreamer :: MonadIO m => URI -> Text -> D.Conn -> m (Maybe StreamerId)
cStreamer url tag d = do
    strmId <- genUUID
    let p = (strmId, url, tag)
    executeW q p d
    r <- rStreamerURL url d
    return $ streamerId <$> r
    where
        q = " \
        \   /* cStreamer */ \
        \   INSERT INTO streamer ( \
        \       streamer_id, \
        \       url, \
        \       tag \
        \   ) \
        \   VALUES (?, ?, ?) \
        \   ON CONFLICT (url) DO UPDATE SET \
        \       tag = tag \
        \ "
--------------------------------------------------------------------------------
lCrawl :: MonadIO m => SiteId -> Cursor -> D.Conn -> m [Crawl]
lCrawl sitId = curse (lCrawlF sitId) (lCrawlN sitId) (lCrawlP sitId)

lCrawlPagePageId :: MonadIO m => CrawlId -> PageId -> PageV ->
    ProcessorId -> D.Conn -> m [PageId]
lCrawlPagePageId (sitId, sitV) pagId pagV procId = queryR q p
    where
        q = " \
        \   /* lCrawlPagePageId */ \
        \   SELECT DISTINCT \
        \       page_id \
        \   FROM \
        \       crawl_page \
        \   WHERE \
        \       (site_id, site_v) = (?, ?) \
        \       AND \
        \       (parent_page_id, parent_page_v, parent_processor_id) = (?, ?, ?) \
        \       AND \
        \       page_v IS NULL \
        \ "
        p = (sitId, sitV, pagId, pagV, procId)

lCrawlPagePageIdEntry :: MonadIO m => CrawlId -> D.Conn -> m [PageId]
lCrawlPagePageIdEntry crwlId = queryR q p
    where
        q = " \
        \   /* lCrawlPagePageIdEntry */ \
        \   SELECT DISTINCT \
        \       page_id \
        \   FROM \
        \       crawl_page \
        \   WHERE \
        \       (site_id, site_v) = (?, ?) \
        \       AND \
        \       parent_page_id IS NULL \
        \       AND \
        \       parent_page_v IS NULL \
        \       AND \
        \       page_v IS NULL \
        \ "
        p = crwlId

lProcessor :: MonadIO m => Cursor -> D.Conn -> m [Processor]
lProcessor = curse lProcessorF lProcessorN lProcessorP

lStreamer :: MonadIO m => Cursor -> D.Conn -> m [Streamer]
lStreamer = curse lStreamerF lStreamerN lStreamerP
--------------------------------------------------------------------------------
rCrawl :: MonadIO m => CrawlId -> D.Conn -> m (Maybe Crawl)
rCrawl crwlId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rCrawl */ \
        \   SELECT * FROM d_crawl_0 \
        \   WHERE \
        \       (site_id, site_v) = (?, ?) \
        \   LIMIT 1 \
        \ "
        p = crwlId

rPageId :: MonadIO m => (SiteId, PageId) -> D.Conn -> m (Maybe Page)
rPageId (sitId, pagId) d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rPageId */ \
        \   SELECT * FROM d_page_0 \
        \   WHERE \
        \       (site_id, page_id) = (?, ?) \
        \   LIMIT 1 \
        \ "
        p = (sitId, pagId)

rPageIns :: MonadIO m => (Site, PageURL) -> D.Conn -> m (Maybe Page)
rPageIns (sit, pURL) d = rPage (siteId sit, pURL) d >>= \case
    Just pag -> return $ Just pag
    Nothing  -> cPageAuto (sit, unPageURL pURL) d >> rPage (siteId sit, pURL) d

rProcessor :: MonadIO m => ProcessorId -> D.Conn -> m (Maybe Processor)
rProcessor procId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rProcessor */ \
        \   SELECT * FROM d_processor_0 \
        \   WHERE \
        \       processor_id = ? \
        \   LIMIT 1 \
        \ "
        p = [procId]

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
rSiteId sitId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rSiteId */ \
        \   SELECT * FROM d_site_0 \
        \   WHERE \
        \       site_id = ? \
        \   LIMIT 1 \
        \ "
        p = [sitId]

rSiteIns :: MonadIO m => SiteURL -> D.Conn -> m (Maybe Site)
rSiteIns sURL d = rSite sURL d >>= \case
    Just sit -> return $ Just sit
    Nothing  -> cSiteAuto (unSiteURL sURL) d >> rSite sURL d

rStreamer :: MonadIO m => StreamerId -> D.Conn -> m (Maybe Streamer)
rStreamer strmId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rStreamer */ \
        \   SELECT * FROM d_streamer_0 \
        \   WHERE \
        \       streamer_id = ? \
        \   LIMIT 1 \
        \ "
        p = [strmId]
--------------------------------------------------------------------------------
uCrawlPage :: (MonadFail m, MonadIO m) => Crawl -> PageId -> PageV ->
    ProcessorId -> [PageId] -> D.Conn -> m ()
uCrawlPage crwl pagId pagV procId pagIds d = do
    let p1 pprocId' = map (crawlSiteId crwl, crawlSiteV crwl,
            pagId, pagV, procId, pprocId',) $
            pagIds ++ [pagId]
    forM_ (crawlProcessorIds crwl) $ \procId' -> executeManyW q1 (p1 procId') d
    executeW q2 p2 d
    where
        q1 = " \
        \   /* uCrawlPage.1 */ \
        \   INSERT INTO crawl_page ( \
        \       site_id, \
        \       site_v, \
        \       parent_page_id, \
        \       parent_page_v, \
        \       parent_processor_id, \
        \       processor_id, \
        \       page_id \
        \   ) \
        \   VALUES (?, ?, ?, ?, ?, ?, ?) \
        \   ON CONFLICT DO NOTHING \
        \ "
        q2 = " \
        \   /* uCrawlPage.2 */ \
        \   UPDATE \
        \       crawl_page \
        \   SET \
        \       (page_v, inserted) = (?, STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')) \
        \   WHERE \
        \       (site_id, site_v, page_id, processor_id) = (?, ?, ?, ?) \
        \       AND \
        \       page_v IS NULL \
        \   "
        p2 = (pagV, crawlSiteId crwl, crawlSiteV crwl, pagId, procId)
--------------------------------------------------------------------------------
dProcessor :: MonadIO m => ProcessorId -> D.Conn -> m ()
dProcessor procId = executeW q p
    where
        q = " \
        \   /* dProcessor */ \
        \   DELETE FROM processor \
        \   WHERE \
        \       processor_id = ? \
        \ "
        p = [procId]

dStreamer :: MonadIO m => StreamerId -> D.Conn -> m ()
dStreamer strmId = executeW q p
    where
        q = " \
        \   /* dStreamer */ \
        \   DELETE FROM streamer \
        \   WHERE \
        \       streamer_id = ? \
        \ "
        p = [strmId]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
lCrawlF :: MonadIO m => SiteId -> Integer -> D.Conn -> m [Crawl]
lCrawlF sitId curLim d = do
    rs <- queryR q p d :: MonadIO m => m [CrawlId']
    catMaybes <$> forM rs (\r -> rCrawl (crawlSiteId' r, crawlSiteV' r) d)
    where
        q = " \
        \   /* lCrawlF */ \
        \   SELECT site_id, site_v FROM crawl \
        \   WHERE \
        \       site_id = ? \
        \   ORDER BY \
        \       site_v DESC \
        \   LIMIT ? \
        \ "
        p = (sitId, curLim)

lCrawlN :: MonadIO m => SiteId -> Integer -> ByteString -> D.Conn -> m [Crawl]
lCrawlN sitId curLim curPosN d = do
    rs <- queryR q p d :: MonadIO m => m [CrawlId']
    catMaybes <$> forM rs (\r -> rCrawl (crawlSiteId' r, crawlSiteV' r) d)
    where
        q = " \
        \   /* lCrawlN */ \
        \   SELECT site_id, site_v FROM crawl \
        \   WHERE \
        \       site_id = ? \
        \       AND \
        \       site_v < ? \
        \   ORDER BY \
        \       site_v DESC \
        \   LIMIT ? \
        \ "
        p = (sitId, curPosN, curLim)

lCrawlP :: MonadIO m => SiteId -> Integer -> ByteString -> D.Conn -> m [Crawl]
lCrawlP sitId curLim curPosP d = do
    rs <- queryR q p d :: MonadIO m => m [CrawlId']
    reverse . catMaybes <$>
        forM rs (\r -> rCrawl (crawlSiteId' r, crawlSiteV' r) d)
    where
        q = " \
        \   /* lCrawlP */ \
        \   SELECT site_id, site_v FROM crawl \
        \   WHERE \
        \       site_id = ? \
        \       AND \
        \       site_v > ? \
        \   ORDER BY \
        \       site_v ASC \
        \   LIMIT ? \
        \ "
        p = (sitId, curPosP, curLim)
--------------------------------------------------------------------------------
lProcessorF :: MonadIO m => Integer -> D.Conn -> m [Processor]
lProcessorF curLim d = do
    procIds <- queryR q p d
    catMaybes <$> forM procIds (`rProcessor` d)
    where
        q = " \
        \   /* lProcessorF */ \
        \   SELECT processor_id FROM processor \
        \   ORDER BY \
        \       inserted DESC, \
        \       processor_id DESC \
        \   LIMIT ? \
        \ "
        p = [curLim]

lProcessorN :: MonadIO m => Integer -> ByteString -> D.Conn -> m [Processor]
lProcessorN curLim curPosN d = do
    procIds <- queryR q p d
    catMaybes <$> forM procIds (`rProcessor` d)
    where
        q = " \
        \   /* lProcessorN */ \
        \   SELECT processor_id FROM processor \
        \   WHERE \
        \       inserted < ? \
        \   ORDER BY \
        \       inserted DESC, \
        \       processor_id DESC \
        \   LIMIT ? \
        \ "
        p = (curPosN, curLim)

lProcessorP :: MonadIO m => Integer -> ByteString -> D.Conn -> m [Processor]
lProcessorP curLim curPosP d = do
    procIds <- queryR q p d
    reverse . catMaybes <$> forM procIds (`rProcessor` d)
    where
        q = " \
        \   /* lProcessorP */ \
        \   SELECT processor_id FROM processor \
        \   WHERE \
        \       inserted > ? \
        \   ORDER BY \
        \       inserted ASC, \
        \       processor_id ASC \
        \   LIMIT ? \
        \ "
        p = (curPosP, curLim)
--------------------------------------------------------------------------------
lStreamerF :: MonadIO m => Integer -> D.Conn -> m [Streamer]
lStreamerF curLim d = do
    strmIds <- queryR q p d
    catMaybes <$> forM strmIds (`rStreamer` d)
    where
        q = " \
        \   /* lStreamerF */ \
        \   SELECT streamer_id FROM streamer \
        \   ORDER BY \
        \       inserted DESC, \
        \       streamer_id DESC \
        \   LIMIT ? \
        \ "
        p = [curLim]

lStreamerN :: MonadIO m => Integer -> ByteString -> D.Conn -> m [Streamer]
lStreamerN curLim curPosN d = do
    strmIds <- queryR q p d
    catMaybes <$> forM strmIds (`rStreamer` d)
    where
        q = " \
        \   /* lStreamerN */ \
        \   SELECT streamer_id FROM streamer \
        \   WHERE \
        \       inserted < ? \
        \   ORDER BY \
        \       inserted DESC, \
        \       streamer_id DESC \
        \   LIMIT ? \
        \ "
        p = (curPosN, curLim)

lStreamerP :: MonadIO m => Integer -> ByteString -> D.Conn -> m [Streamer]
lStreamerP curLim curPosP d = do
    strmIds <- queryR q p d
    reverse . catMaybes <$> forM strmIds (`rStreamer` d)
    where
        q = " \
        \   /* lStreamerP */ \
        \   SELECT streamer_id FROM streamer \
        \   WHERE \
        \       inserted > ? \
        \   ORDER BY \
        \       inserted ASC, \
        \       streamer_id ASC \
        \   LIMIT ? \
        \ "
        p = (curPosP, curLim)
--------------------------------------------------------------------------------
cPageAuto :: MonadIO m => (Site, URI) -> D.Conn -> m (Maybe PageURL)
cPageAuto (sit, url) d = do
    executeW q p d
    return $ Just $ PageURL url
    where
        q = " \
        \   /* cPageAuto */ \
        \   INSERT INTO pages ( \
        \       page_id, \
        \       site_id, \
        \       url \
        \   ) \
        \   VALUES (?, ?, ?) \
        \   ON CONFLICT DO NOTHING \
        \ "
        url' = toText $ show (siteURL sit) ++ show url
        pagId = PageId $ hash url'
        p = (pagId, siteId sit, url)

cSiteAuto :: MonadIO m => URI -> D.Conn -> m (Maybe SiteURL)
cSiteAuto url d = do
    executeW q p d
    return $ Just $ SiteURL url
    where
        q = " \
        \   /* cSiteAuto */ \
        \   INSERT INTO site ( \
        \       site_id, \
        \       url, \
        \       auto \
        \   ) \
        \   VALUES (?, ?, true) \
        \   ON CONFLICT (site_id) DO UPDATE SET \
        \       auto = true \
        \ "
        sitId = SiteId $ hash $ show url
        p = (sitId, url)

rPage :: MonadIO m => (SiteId, PageURL) -> D.Conn -> m (Maybe Page)
rPage (sitId, pURL) d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rPage */ \
        \   SELECT * FROM d_page_0 \
        \   WHERE \
        \       (site_id, url) = (?, ?) \
        \   LIMIT 1 \
        \ "
        p = (sitId, pURL)

rProcessorURL :: MonadIO m => URI -> D.Conn -> m (Maybe Processor)
rProcessorURL url d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rProcessorURL */ \
        \   SELECT * FROM d_processor_0 \
        \   WHERE \
        \       url = ? \
        \   LIMIT 1 \
        \ "
        p = [url]

rStreamerURL :: MonadIO m => URI -> D.Conn -> m (Maybe Streamer)
rStreamerURL url d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rStreamerURL */ \
        \   SELECT * FROM d_streamer_0 \
        \   WHERE \
        \       url = ? \
        \   LIMIT 1 \
        \ "
        p = [url]

hash :: Text -> Hash.Digest Hash.SHA256
hash t = Hash.hash (encodeUtf8 t :: ByteString)
