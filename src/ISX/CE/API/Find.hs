module ISX.CE.API.Find (
    fPlugProc,
    fPlugProcHref,
    fPlugProcs,
    fPlugStrm,
    fPlugStrmHref,
    fPlugStrms,
    fSite,
    fSites,
    ) where


import           ISX.CE.API.Auth
import           ISX.CE.API.Href
import           Snap.Core              hiding (pass)
import           TPX.Com.Snap.CoreUtils
import qualified ISX.CE.DB              as D


fPlugProc :: MonadSnap m => APerm -> D.Conn ->
    MaybeT m (D.PlugProc, D.PlugProcId)
fPlugProc _ d = do
    Just pId_ <- lift $ getParam "plug_proc_id"
    Just pId  <- return $ toRouteId pId_
    Just p <- D.rPlugProc pId d
    return (p, D.plugProcId p)

fPlugProcHref :: MonadSnap m => Maybe PlugProcHref -> APerm ->
    D.Conn -> MaybeT m (D.PlugProc, D.PlugProcId)
fPlugProcHref pH _ d = do
    Just pId <- return $ fromRouteHref =<< pH
    Just p <- D.rPlugProc pId d
    return (p, D.plugProcId p)

fPlugProcs :: MonadSnap m => APerm -> D.Conn -> MaybeT m ()
fPlugProcs _ _ = pass

fPlugStrm :: MonadSnap m => APerm -> D.Conn ->
    MaybeT m (D.PlugStrm, D.PlugStrmId)
fPlugStrm _ d = do
    Just pId_ <- lift $ getParam "plug_strm_id"
    Just pId  <- return $ toRouteId pId_
    Just p <- D.rPlugStrm pId d
    return (p, D.plugStrmId p)

fPlugStrmHref :: MonadSnap m => Maybe PlugStrmHref -> APerm ->
    D.Conn -> MaybeT m (D.PlugStrm, D.PlugStrmId)
fPlugStrmHref pH _ d = do
    Just pId <- return $ fromRouteHref =<< pH
    Just p <- D.rPlugStrm pId d
    return (p, D.plugStrmId p)

fPlugStrms :: MonadSnap m => APerm -> D.Conn -> MaybeT m ()
fPlugStrms _ _ = pass

fSite :: MonadSnap m => APerm -> D.Conn -> MaybeT m (D.Site, D.SiteId)
fSite AR d = do
    Just sURL_ <- lift $ getParam "site_id"
    Just sURL  <- return $ toRouteId sURL_
    Just s <- D.rSite sURL d
    return (s, D.siteId s)
fSite AW _ = fail "fSite/AW not allowed"

fSites :: MonadSnap m => APerm -> D.Conn -> MaybeT m ()
fSites AR _ = fail "fSites/AR not allowed"
fSites AW _ = pass
