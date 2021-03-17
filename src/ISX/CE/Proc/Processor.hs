module ISX.CE.Proc.Processor (process) where


import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           ISX.CE.API.Href
import           TPX.Com.Isoxya.PlugProc
import           TPX.Com.Snap.CoreUtils
import           TPX.Com.URI
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified ISX.CE.DB               as D
import qualified ISX.CE.Msg              as M
import qualified Network.HTTP.Conduit    as HTTP
import qualified Network.HTTP.Types      as HTTP
import qualified TPX.Com.Log             as L
import qualified TPX.Com.Net             as N


process :: L.Logger -> M.ChanStrm -> M.ChanCrwl -> N.Conn -> D.Conn ->
    M.MsgProc -> IO ()
process l mChStrm mChCrwl n d (ppId, msg) = do
    Just pp <- D.rPlugProc ppId d
    L.debug l $ show pp
    Just c <- D.rCrwl (M.crwlPageSiteId msg, M.crwlPageSiteV msg) d
    L.debug l $ show c
    Just s <- D.rSiteId (D.crwlSiteId c) d
    L.debug l $ show s
    Just p <- D.rPageId (D.crwlSiteId c, M.crwlPagePageId msg) d
    let tx = genPlugProcI msg pp c s p
    L.debug l $ show tx
    let req = N.jsonReq $
            N.makeReq' "POST" (D.unPlugProcURL $ D.plugProcURL pp) (encode tx)
    L.debug l $ show req
    L.debug l $ decodeUtf8 $ encode tx
    res <- N.makeRes req n
    L.debug l $ show res
    L.debug l $ decodeUtf8 $ HTTP.responseBody res
    let Just rx = decode $ HTTP.responseBody res :: Maybe PlugProcO
    pIdsInt <- D.urlsPageIds s (S.map unURIReference $ plugProcOURLs rx) d
    D.uCrwlPage c (M.crwlPagePageId msg) (M.crwlPagePageV msg)
        (D.plugProcId pp) pIdsInt d
    pIds <- D.lCrwlPagePageId (D.crwlId c) (M.crwlPagePageId msg)
        (M.crwlPagePageV msg) (D.plugProcId pp) d
    _ <- M.txCrwlPageIds s c pIds mChCrwl
    _ <- M.txCrwlPageData c msg pp (plugProcOData rx) mChStrm
    L.info l $ decodeUtf8 (unCrwlHref $
            toRouteHref (D.siteURL s, D.crwlSiteV c)) <> " PROC " <>
        show (D.unPlugProcId $ D.plugProcId pp) <> " " <>
        show (D.unSiteURL $ D.siteURL s) <>
        show (D.unPageURL $ D.pageURL p) <> " " <>
        show (HTTP.statusCode $ HTTP.responseStatus res)


genPlugProcI :: M.CrwlPage -> D.PlugProc -> D.Crwl -> D.Site -> D.Page ->
    PlugProcI
genPlugProcI msg pp c s p = PlugProcI meta header body
    where
    url = URIAbsolute $ D.unSiteURL $ D.pageURLAbs (D.siteURL s) (D.pageURL p)
    method = decodeUtf8 <$> M.crwlPageReqMethod $ M.crwlPageReq msg
    status = case M.crwlPageRes msg of
        Right r -> Just $ toInteger $ M.crwlPageResStatus r
        _       -> empty
    duration = case M.crwlPageRes msg of
        Right r -> Just $ M.crwlPageResDuration r
        _       -> empty
    err = case M.crwlPageRes msg of
        Left e  -> Just $ show e
        _       -> empty
    config = D.crwlPlugProcConf c ^? key (D.plugProcTag pp)
    meta = PlugProcIMeta {
        plugProcIMetaURL      = url,
        plugProcIMetaMethod   = method,
        plugProcIMetaStatus   = status,
        plugProcIMetaDuration = duration,
        plugProcIMetaErr      = err,
        plugProcIMetaConfig   = config}
    header = case M.crwlPageBlob msg of
        Just b  -> M.crwlPageBlobHeader b
        Nothing -> M.empty
    body = case M.crwlPageBlob msg of
        Just b  -> toStrict $ M.crwlPageBlobBody b
        Nothing -> ""
