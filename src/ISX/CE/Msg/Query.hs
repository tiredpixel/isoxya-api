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
genCrwlPageId :: D.CrwlId -> D.PageId -> CrwlPageId
genCrwlPageId (sId, sV) = CrwlPageId sId sV

genCrwlPageErr :: D.CrwlId -> D.PageId -> HTTP.Request ->
    CrwlPageResErr -> UTCTime -> CrwlPage
genCrwlPageErr (sId, sV) pId req resErr t = CrwlPage
        sId sV pId (D.PageV t) req' (Left resErr) Nothing
    where
        req' = CrwlPageReq (HTTP.method req) (HTTP.requestVersion req)

genCrwlPageRes :: D.CrwlId -> D.PageId -> HTTP.Request ->
    HTTP.Response LByteString -> UTCTime -> UTCTime -> CrwlPage
genCrwlPageRes (sId, sV) pId req res t t' = CrwlPage
        sId sV pId (D.PageV t) req' (Right res') (Just blob)
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

txCrwlPageIds :: MonadIO m => D.SiteId -> D.CrwlId -> [D.PageId] -> ChanCrwl ->
    m ()
txCrwlPageIds sId cId pIds mCh = liftIO $ writeList2Chan mCh msgs
    where
        msgs = [(sId, genCrwlPageId cId pId) | pId <- pIds]
--------------------------------------------------------------------------------
rx :: Chan a -> (a -> IO ()) -> IO ()
rx mCh f = getChanContents mCh >>= mapM_ f
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
hConvert :: (CI.CI ByteString, ByteString) -> (Text, Text)
hConvert (k, v) = (decodeUtf8 $ CI.original k, decodeUtf8 v)
