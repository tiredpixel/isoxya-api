module ISX.CE.Msg.Query (
    genCrwlPageErr,
    genCrwlPageRes,
    --
    txCrwlPage,
    txCrwlPageIds,
    --
    rx,
    ) where


import           Control.Concurrent.Chan
import           Data.Time.Clock
import           ISX.CE.Msg.Types
import qualified Data.CaseInsensitive      as CI
import qualified Data.Map                  as M
import qualified ISX.CE.DB                 as D
import qualified Network.HTTP.Conduit      as HTTP
import qualified Network.HTTP.Types.Status as HTTP
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
genCrwlPageErr :: D.CrwlId -> D.PageId -> HTTP.Request -> CrwlPageResErr ->
    UTCTime -> CrwlPage
genCrwlPageErr (sId, sV) pId req resErr t = CrwlPage {
        crwlPageSiteId = sId,
        crwlPageSiteV  = sV,
        crwlPagePageId = pId,
        crwlPagePageV  = D.PageV t,
        crwlPageReq    = pageReq,
        crwlPageRes    = Left resErr,
        crwlPageBlob   = Nothing}
    where
        pageReq = CrwlPageReq (HTTP.method req) (HTTP.requestVersion req)

genCrwlPageRes :: D.CrwlId -> D.PageId -> HTTP.Request ->
    HTTP.Response LByteString -> UTCTime -> UTCTime -> CrwlPage
genCrwlPageRes (sId, sV) pId req res t t' = CrwlPage {
        crwlPageSiteId = sId,
        crwlPageSiteV  = sV,
        crwlPagePageId = pId,
        crwlPagePageV  = D.PageV t,
        crwlPageReq    = pageReq,
        crwlPageRes    = Right pageRes,
        crwlPageBlob   = Just pageBlob}
    where
        hConvert :: (CI.CI ByteString, ByteString) -> (Text, Text)
        hConvert (k, v) = (decodeUtf8 $ CI.original k, decodeUtf8 v)
        pageReq = CrwlPageReq (HTTP.method req) (HTTP.requestVersion req)
        pageRes = CrwlPageRes (HTTP.statusCode $ HTTP.responseStatus res)
            (HTTP.responseVersion res) (toRational $ diffUTCTime t' t)
        pageBlob = CrwlPageBlob
            (M.fromList (hConvert <$> HTTP.responseHeaders res))
            (HTTP.responseBody res)
--------------------------------------------------------------------------------
txCrwlPage :: MonadIO m => D.Crwl -> CrwlPage -> ChanProc -> m ()
txCrwlPage c cp mCh = liftIO $ writeList2Chan mCh msgs
    where
        msgs = [(ppId, cp) | ppId <- D.crwlPlugProcIds c]

txCrwlPageIds :: MonadIO m => D.SiteId -> D.Crwl -> [D.PageId] -> ChanCrwl ->
    m ()
txCrwlPageIds sId c pIds mCh = liftIO $ writeList2Chan mCh msgs
    where
        cpi pId = CrwlPageId {
            crwlPageIdSiteId = D.crwlSiteId c,
            crwlPageIdSiteV  = D.crwlSiteV c,
            crwlPageIdPageId = pId}
        msgs = [(sId, cpi pId) | pId <- pIds]
--------------------------------------------------------------------------------
rx :: Chan a -> (a -> IO ()) -> IO ()
rx mCh f = getChanContents mCh >>= mapM_ f
