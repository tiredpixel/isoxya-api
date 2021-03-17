module ISX.CE.Msg.Query (
    genCrwlPageErr,
    genCrwlPageRes,
    --
    txCrwlPage,
    txCrwlPageData,
    txCrwlPageIds,
    --
    rx,
    ) where


import           Control.Concurrent.Chan
import           Data.Time.Clock
import           ISX.CE.Msg.Types
import qualified Data.Aeson                as A
import qualified Data.CaseInsensitive      as CI
import qualified Data.Map                  as M
import qualified ISX.CE.DB                 as D
import qualified Network.HTTP.Conduit      as HTTP
import qualified Network.HTTP.Types.Status as HTTP
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
genCrwlPageId :: D.Crwl -> D.PageId -> CrwlPageId
genCrwlPageId c = CrwlPageId (D.crwlSiteId c) (D.crwlSiteV c)

genCrwlPageData :: D.Crwl -> CrwlPage -> D.PlugProc -> A.Value -> CrwlPageData
genCrwlPageData c cp pp = CrwlPageData (D.crwlSiteId c) (D.crwlSiteV c)
        (crwlPagePageId cp) (crwlPagePageV cp) (D.plugProcId pp)

genCrwlPageErr :: D.Crwl -> D.PageId -> HTTP.Request ->
    CrwlPageResErr -> UTCTime -> CrwlPage
genCrwlPageErr c pId req resErr t = CrwlPage (D.crwlSiteId c) (D.crwlSiteV c)
        pId (D.PageV t) req' (Left resErr) Nothing
    where
        req' = CrwlPageReq (HTTP.method req) (HTTP.requestVersion req)

genCrwlPageRes :: D.Crwl -> D.PageId -> HTTP.Request ->
    HTTP.Response LByteString -> UTCTime -> UTCTime -> CrwlPage
genCrwlPageRes c pId req res t t' = CrwlPage (D.crwlSiteId c) (D.crwlSiteV c)
        pId (D.PageV t) req' (Right res') (Just blob)
    where
        req' = CrwlPageReq (HTTP.method req) (HTTP.requestVersion req)
        res' = CrwlPageRes (HTTP.statusCode $ HTTP.responseStatus res)
            (HTTP.responseVersion res) (toRational $ diffUTCTime t' t)
        blob = CrwlPageBlob (M.fromList (hConvert <$> HTTP.responseHeaders res))
            (HTTP.responseBody res)
--------------------------------------------------------------------------------
txCrwlPage :: MonadIO m => D.Crwl -> CrwlPage -> ChanProc -> m ()
txCrwlPage c cp mCh = liftIO $ writeList2Chan mCh msgs
    where
        msgs = [(ppId, cp) | ppId <- D.crwlPlugProcIds c]

txCrwlPageData :: MonadIO m => D.Crwl -> CrwlPage -> D.PlugProc -> A.Value ->
    ChanStrm -> m ()
txCrwlPageData c cp pp dat mCh = liftIO $ writeList2Chan mCh msgs
    where
        cpd = genCrwlPageData c cp pp dat
        msgs = [(psId, cpd) | psId <- D.crwlPlugStrmIds c]

txCrwlPageIds :: MonadIO m => D.Site -> D.Crwl -> [D.PageId] -> ChanCrwl -> m ()
txCrwlPageIds s c pIds mCh = liftIO $ writeList2Chan mCh msgs
    where
        msgs = [(D.siteId s, genCrwlPageId c pId) | pId <- pIds]
--------------------------------------------------------------------------------
rx :: Chan a -> (a -> IO ()) -> IO ()
rx mCh f = getChanContents mCh >>= mapM_ f
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
hConvert :: (CI.CI ByteString, ByteString) -> (Text, Text)
hConvert (k, v) = (decodeUtf8 $ CI.original k, decodeUtf8 v)
