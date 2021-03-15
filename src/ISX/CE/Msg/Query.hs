module ISX.CE.Msg.Query (
    txCrwlPageIds,
    --
    ) where


import           Control.Concurrent.Chan
import           ISX.CE.Msg.Types
import qualified ISX.CE.DB               as D
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
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
