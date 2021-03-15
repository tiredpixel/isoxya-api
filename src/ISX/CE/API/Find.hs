module ISX.CE.API.Find (
    fSite,
    fSites,
    ) where


import           ISX.CE.API.Auth
import           ISX.CE.API.Href        ()
import           Snap.Core              hiding (pass)
import           TPX.Com.Snap.CoreUtils
import qualified ISX.CE.DB              as D


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
