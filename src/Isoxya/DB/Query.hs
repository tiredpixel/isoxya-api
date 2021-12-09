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
cCrawl :: MonadIO m =>
    SiteId ->
    A.Value -> [ProcessorId] -> [StreamerId] -> D.Conn -> m (Maybe SiteV)
cCrawl stId conf proIds strIds d = withTransaction d $ do
    t <- liftIO getCurrentTime
    let stV_ = Just $ SiteV t
    let p0 = (stId, stV_, A.encode conf)
    executeW q0 p0 d
    let p1 = map (stId, stV_,) proIds
    executeManyW q1 p1 d
    let p2 = map (stId, stV_,) strIds
    executeManyW q2 p2 d
    return stV_
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

cCrawlPagesEntry :: MonadIO m =>
    CrawlId -> [ProcessorId] -> [PageId] -> D.Conn -> m [()]
cCrawlPagesEntry (stId, stV) proIds pgIds d = forM proIds $ \proId ->
    executeManyW q (p proId) d
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
        p proId = map (stId, stV, proId,) pgIds

cProcessor :: MonadIO m =>
    URI -> Text -> D.Conn -> m (Maybe ProcessorId)
cProcessor url tag d = do
    proId <- genUUID
    let p = (proId, url, tag)
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

cSite :: MonadIO m =>
    URI -> D.Conn -> m (Maybe SiteURL)
cSite url d = do
    executeW q p d
    r <- rSiteId stId d
    return $ siteURL <$> r
    where
        stId = SiteId $ hash $ show url
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
        p = (stId, url)

cStreamer :: MonadIO m =>
    URI -> Text -> D.Conn -> m (Maybe StreamerId)
cStreamer url tag d = do
    strId <- genUUID
    let p = (strId, url, tag)
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
lCrawl :: MonadIO m =>
    SiteId -> Cursor -> D.Conn -> m [Crawl]
lCrawl stId = curse (lCrawlF stId) (lCrawlN stId) (lCrawlP stId)

lCrawlPagePageId :: MonadIO m =>
    CrawlId -> PageId -> PageV -> ProcessorId -> D.Conn -> m [PageId]
lCrawlPagePageId (stId, stV) pgId pgV proId = queryR q p
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
        p = (stId, stV, pgId, pgV, proId)

lCrawlPagePageIdEntry :: MonadIO m =>
    CrawlId -> D.Conn -> m [PageId]
lCrawlPagePageIdEntry crlId = queryR q p
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
        p = crlId

lProcessor :: MonadIO m =>
    Cursor -> D.Conn -> m [Processor]
lProcessor = curse lProcessorF lProcessorN lProcessorP

lStreamer :: MonadIO m =>
    Cursor -> D.Conn -> m [Streamer]
lStreamer = curse lStreamerF lStreamerN lStreamerP
--------------------------------------------------------------------------------
rCrawl :: MonadIO m =>
    CrawlId -> D.Conn -> m (Maybe Crawl)
rCrawl crlId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rCrawl */ \
        \   SELECT * FROM d_crawl_0 \
        \   WHERE \
        \       (site_id, site_v) = (?, ?) \
        \   LIMIT 1 \
        \ "
        p = crlId

rPageId :: MonadIO m =>
    (SiteId, PageId) -> D.Conn -> m (Maybe Page)
rPageId (stId, pgId) d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rPageId */ \
        \   SELECT * FROM d_page_0 \
        \   WHERE \
        \       (site_id, page_id) = (?, ?) \
        \   LIMIT 1 \
        \ "
        p = (stId, pgId)

rPageIns :: MonadIO m =>
    (Site, PageURL) -> D.Conn -> m (Maybe Page)
rPageIns (st, pgURL) d = rPage (siteId st, pgURL) d >>= \case
    Just pag -> return $ Just pag
    Nothing  -> cPageAuto (st, unPageURL pgURL) d >> rPage (siteId st, pgURL) d

rProcessor :: MonadIO m =>
    ProcessorId -> D.Conn -> m (Maybe Processor)
rProcessor proId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rProcessor */ \
        \   SELECT * FROM d_processor_0 \
        \   WHERE \
        \       processor_id = ? \
        \   LIMIT 1 \
        \ "
        p = [proId]

rSite :: MonadIO m =>
    SiteURL -> D.Conn -> m (Maybe Site)
rSite stURL d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rSite */ \
        \   SELECT * FROM d_site_0 \
        \   WHERE \
        \       url = ? \
        \   LIMIT 1 \
        \ "
        p = [stURL]

rSiteId :: MonadIO m =>
    SiteId -> D.Conn -> m (Maybe Site)
rSiteId stId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rSiteId */ \
        \   SELECT * FROM d_site_0 \
        \   WHERE \
        \       site_id = ? \
        \   LIMIT 1 \
        \ "
        p = [stId]

rSiteIns :: MonadIO m =>
    SiteURL -> D.Conn -> m (Maybe Site)
rSiteIns stURL d = rSite stURL d >>= \case
    Just st -> return $ Just st
    Nothing -> cSiteAuto (unSiteURL stURL) d >> rSite stURL d

rStreamer :: MonadIO m =>
    StreamerId -> D.Conn -> m (Maybe Streamer)
rStreamer strId d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rStreamer */ \
        \   SELECT * FROM d_streamer_0 \
        \   WHERE \
        \       streamer_id = ? \
        \   LIMIT 1 \
        \ "
        p = [strId]
--------------------------------------------------------------------------------
uCrawlPage :: (MonadFail m, MonadIO m) =>
    Crawl -> PageId -> PageV -> ProcessorId -> [PageId] -> D.Conn -> m ()
uCrawlPage crl pgId pgV proId pgIds d = do
    let p1 pproId' = map (crawlSiteId crl, crawlSiteV crl,
            pgId, pgV, proId, pproId',) $
            pgIds ++ [pgId]
    forM_ (crawlProcessorIds crl) $ \proId' -> executeManyW q1 (p1 proId') d
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
        p2 = (pgV, crawlSiteId crl, crawlSiteV crl, pgId, proId)
--------------------------------------------------------------------------------
dProcessor :: MonadIO m =>
    ProcessorId -> D.Conn -> m ()
dProcessor proId = executeW q p
    where
        q = " \
        \   /* dProcessor */ \
        \   DELETE FROM processor \
        \   WHERE \
        \       processor_id = ? \
        \ "
        p = [proId]

dStreamer :: MonadIO m =>
    StreamerId -> D.Conn -> m ()
dStreamer strId = executeW q p
    where
        q = " \
        \   /* dStreamer */ \
        \   DELETE FROM streamer \
        \   WHERE \
        \       streamer_id = ? \
        \ "
        p = [strId]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
lCrawlF :: MonadIO m =>
    SiteId -> Integer -> D.Conn -> m [Crawl]
lCrawlF stId curLim d = do
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
        p = (stId, curLim)

lCrawlN :: MonadIO m =>
    SiteId -> Integer -> ByteString -> D.Conn -> m [Crawl]
lCrawlN stId curLim curPosN d = do
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
        p = (stId, curPosN, curLim)

lCrawlP :: MonadIO m =>
    SiteId -> Integer -> ByteString -> D.Conn -> m [Crawl]
lCrawlP stId curLim curPosP d = do
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
        p = (stId, curPosP, curLim)
--------------------------------------------------------------------------------
lProcessorF :: MonadIO m =>
    Integer -> D.Conn -> m [Processor]
lProcessorF curLim d = do
    proIds <- queryR q p d
    catMaybes <$> forM proIds (`rProcessor` d)
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

lProcessorN :: MonadIO m =>
    Integer -> ByteString -> D.Conn -> m [Processor]
lProcessorN curLim curPosN d = do
    proIds <- queryR q p d
    catMaybes <$> forM proIds (`rProcessor` d)
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

lProcessorP :: MonadIO m =>
    Integer -> ByteString -> D.Conn -> m [Processor]
lProcessorP curLim curPosP d = do
    proIds <- queryR q p d
    reverse . catMaybes <$> forM proIds (`rProcessor` d)
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
lStreamerF :: MonadIO m =>
    Integer -> D.Conn -> m [Streamer]
lStreamerF curLim d = do
    strIds <- queryR q p d
    catMaybes <$> forM strIds (`rStreamer` d)
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

lStreamerN :: MonadIO m =>
    Integer -> ByteString -> D.Conn -> m [Streamer]
lStreamerN curLim curPosN d = do
    strIds <- queryR q p d
    catMaybes <$> forM strIds (`rStreamer` d)
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

lStreamerP :: MonadIO m =>
    Integer -> ByteString -> D.Conn -> m [Streamer]
lStreamerP curLim curPosP d = do
    strIds <- queryR q p d
    reverse . catMaybes <$> forM strIds (`rStreamer` d)
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
cPageAuto :: MonadIO m =>
    (Site, URI) -> D.Conn -> m (Maybe PageURL)
cPageAuto (st, url) d = do
    executeW q p d
    return $ Just $ PageURL url
    where
        url' = toText $ show (siteURL st) ++ show url
        pgId = PageId $ hash url'
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
        p = (pgId, siteId st, url)

cSiteAuto :: MonadIO m =>
    URI -> D.Conn -> m (Maybe SiteURL)
cSiteAuto url d = do
    executeW q p d
    return $ Just $ SiteURL url
    where
        stId = SiteId $ hash $ show url
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
        p = (stId, url)

rPage :: MonadIO m =>
    (SiteId, PageURL) -> D.Conn -> m (Maybe Page)
rPage (stId, pgURL) d = listToMaybe <$> queryR q p d
    where
        q = " \
        \   /* rPage */ \
        \   SELECT * FROM d_page_0 \
        \   WHERE \
        \       (site_id, url) = (?, ?) \
        \   LIMIT 1 \
        \ "
        p = (stId, pgURL)

rProcessorURL :: MonadIO m =>
    URI -> D.Conn -> m (Maybe Processor)
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

rStreamerURL :: MonadIO m =>
    URI -> D.Conn -> m (Maybe Streamer)
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

hash ::
    Text -> Hash.Digest Hash.SHA256
hash t = Hash.hash (encodeUtf8 t :: ByteString)
