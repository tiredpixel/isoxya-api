module ISX.CE.Msg.Query (
    txCrwlPage,
    txCrwlPageIds,
    --
    rx,
    ) where


import           Control.Concurrent.Chan
import           ISX.CE.Msg.Types
import qualified ISX.CE.DB               as D
--------------------------------------------------------------------------------
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
