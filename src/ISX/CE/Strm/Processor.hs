module ISX.CE.Strm.Processor (process) where


import           Data.Aeson
import           ISX.CE.API.Href
import           TPX.Com.Isoxya.PlugStrm
import           TPX.Com.Snap.CoreUtils
import           TPX.Com.URI
import qualified ISX.CE.DB               as D
import qualified ISX.CE.Msg              as M
import qualified Network.HTTP.Conduit    as HTTP
import qualified Network.HTTP.Types      as HTTP
import qualified TPX.Com.Log             as L
import qualified TPX.Com.Net             as N


process :: L.Logger -> N.Conn -> D.Conn -> M.MsgStrm -> IO ()
process l n d (psId, msg) = do
    Just ps <- D.rPlugStrm psId d
    L.debug l $ show ps
    Just pp <- D.rPlugProc (M.crwlPageDataPlugProcId msg) d
    L.debug l $ show pp
    Just c <- D.rCrwl (M.crwlPageDataSiteId msg, M.crwlPageDataSiteV msg) d
    L.debug l $ show c
    Just s <- D.rSiteId (D.crwlSiteId c) d
    L.debug l $ show s
    Just p <- D.rPageId (D.crwlSiteId c, M.crwlPageDataPageId msg) d
    let tx = genPlugStrm msg pp c s p
    L.debug l $ show tx
    let req = N.jsonReq $
            N.makeReq' "POST" (D.unPlugStrmURL $ D.plugStrmURL ps) (encode tx)
    L.debug l $ show req
    L.debug l $ decodeUtf8 $ encode tx
    res <- N.makeRes req n
    L.debug l $ show res
    L.info l $ decodeUtf8 (unCrwlHref $
            toRouteHref (D.siteURL s, D.crwlSiteV c)) <> " STRM " <>
        show (D.unPlugStrmId $ D.plugStrmId ps) <> " " <>
        show (D.unSiteURL $ D.siteURL s) <>
        show (D.unPageURL $ D.pageURL p) <> " " <>
        show (HTTP.statusCode $ HTTP.responseStatus res)


genPlugStrm :: M.CrwlPageData -> D.PlugProc -> D.Crwl -> D.Site -> D.Page ->
    PlugStrm
genPlugStrm msg pp c s p = PlugStrm {
    plugStrmCrwlHref     = crwlHref,
    plugStrmCrwlTBegin   = D.unSiteV $ D.crwlSiteV c,
    plugStrmSiteHref     = decodeUtf8 $ unSiteHref $ toRouteHref (D.siteURL s),
    plugStrmSiteURL      = URIAbsolute $ D.unSiteURL $ D.siteURL s,
    plugStrmOrgHref      = orgHref,
    plugStrmPlugProcHref = plugProcHref,
    plugStrmPlugProcTag  = D.plugProcTag pp,
    plugStrmURL          = url,
    plugStrmTRetrieval   = D.unPageV $ M.crwlPageDataPageV msg,
    plugStrmData         = M.crwlPageDataData msg}
    where
        crwlHref = decodeUtf8 $ unCrwlHref $
            toRouteHref (D.siteURL s, D.crwlSiteV c)
        orgHref = "/org/00000000-0000-0000-0000-000000000000"
        plugProcHref = decodeUtf8 $ unPlugProcHref $
            toRouteHref (D.plugProcId pp)
        url = URIAbsolute $ D.unSiteURL $
            D.pageURLAbs (D.siteURL s) (D.pageURL p)
