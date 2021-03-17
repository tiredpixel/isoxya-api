module ISX.CE.Crwl.Processor (process) where


import           Control.Concurrent     (threadDelay)
import           Control.Exception      (handle)
import           Data.Time.Clock
import           ISX.CE.API.Href
import           TPX.Com.Snap.CoreUtils
import qualified ISX.CE.DB              as D
import qualified ISX.CE.Msg             as M
import qualified Network.HTTP.Conduit   as HTTP
import qualified TPX.Com.Log            as L
import qualified TPX.Com.Net            as N
import qualified Text.Regex             as R


process :: L.Logger -> Text -> M.ChanProc -> N.Conn -> D.Conn -> M.MsgCrwl ->
    IO ()
process l ver mCh n d (sId, msg) = do
    Just s <- D.rSiteId sId d
    L.debug l $ show s
    Just c <- D.rCrwl (M.crwlPageIdSiteId msg, M.crwlPageIdSiteV msg) d
    L.debug l $ show c
    Just p <- D.rPageId (D.crwlSiteId c, M.crwlPageIdPageId msg) d
    let reqURL = D.pageURLAbs (D.siteURL s) (D.pageURL p)
    let req = N.userAgentReq (encodeUtf8 $ userAgentStr ver) $
            N.makeReq "GET" (D.unSiteURL reqURL) ""
    L.debug l $ show req
    t <- getCurrentTime
    cp <- handle (httpH l c (D.pageId p) req t) $ do
        res <- N.makeResLim reqLim req n
        L.debug l $ show res
        M.genCrwlPageRes c (D.pageId p) req res t <$> getCurrentTime
    L.debug l $ show cp
    _ <- M.txCrwlPage c cp mCh
    L.info l $ decodeUtf8 (unCrwlHref $
            toRouteHref (D.siteURL s, D.crwlSiteV c)) <> " CRWL " <>
        decodeUtf8 (M.crwlPageReqMethod $ M.crwlPageReq cp) <> " " <>
        show (D.unSiteURL $ D.siteURL s) <>
        show (D.unPageURL $ D.pageURL p) <> " " <>
        M.showCrwlPageRes (M.crwlPageRes cp)
    L.debug l $ "LIMITING " <> show rateLim <> " μs"
    threadDelay rateLim
    where
        reqLim = 1048576 -- 1 MB
        rateLim = 10000000 -- 10 s


httpH :: L.Logger -> D.Crwl -> D.PageId -> HTTP.Request -> UTCTime ->
    HTTP.HttpException -> IO M.CrwlPage
httpH l c pId req t ex = do
    L.err l $ show ex
    return $ M.genCrwlPageErr c pId req (M.httpExResErr ex) t

userAgentStr :: Text -> Text
userAgentStr ver = toText $ R.subRegex r str (toString ver)
    where
        str = "Isoxya/${VERSION} (+https://www.isoxya.com/)"
        r = R.mkRegex "\\$\\{VERSION\\}"
