module ISX.CE.API.Zone.Crwl (
    siteCreate,
    siteList,
    siteRead,
    ) where


import           ISX.CE.API.Core
import qualified ISX.CE.DB       as D
import qualified ISX.CE.Msg      as M


siteCreate :: Handler b API ()
siteCreate = do
    mChCrwl <- gets _msgCrwl
    d <- gets _db
    Just (s, sId) <- run notFound $ fCrwls AW d
    req_     <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just pps <- run notFound $ mapM (\pH ->
        fPlugProcHref (Just pH) AR d) (crwlCPlugProcHrefs req)
    Just pss <- run notFound $ mapM (\pH ->
        fPlugStrmHref (Just pH) AR d) (crwlCPlugStrmHrefs req)
    Just sV <- D.cCrwl sId (crwlCPlugProcConf req)
        (snd <$> pps) (snd <$> pss) d
    Just c <- D.rCrwl (sId, sV) d
    _ <- D.cEntryURLs s c d
    pIds <- D.lCrwlPagePageIdEntry (sId, sV) d
    M.txCrwlPageIds sId (D.crwlId c) pIds mChCrwl
    let r = crwl s c
    created (unCrwlHref $ crwlHref r) r

siteList :: Handler b API ()
siteList = do
    d <- gets _db
    Just (s, sId) <- run notFound $ fCrwls AR d
    cur <- parseReq
    cs <- D.lCrwl sId cur d
    setResLink (unCrwlsHref (toRouteHref (D.siteURL s) :: CrwlsHref))
        (formatTime . D.unSiteV . D.crwlSiteV) cs
    rs <- forM cs $ \c -> do
        Just s' <- D.rSiteId (D.crwlSiteId c) d
        return $ crwl s' c
    writeJSON rs

siteRead :: Handler b API ()
siteRead = do
    d <- gets _db
    Just ((s, c), _) <- run notFound $ fCrwl AR d
    writeJSON $ crwl s c
