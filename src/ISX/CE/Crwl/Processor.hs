module ISX.CE.Crwl.Processor (process) where


import           Control.Concurrent        (threadDelay)
import           Control.Exception         (handle)
import           Data.Time.Clock
import           ISX.CE.API.Href
import           TPX.Com.Snap.CoreUtils
import qualified Data.CaseInsensitive      as CI
import qualified Data.Map                  as M
import qualified ISX.CE.DB                 as D
import qualified ISX.CE.Msg                as M
import qualified Network.HTTP.Conduit      as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified TPX.Com.Log               as L
import qualified TPX.Com.Net               as N
import qualified Text.Regex                as R


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
    cp <- handle (httpH l (D.crwlId c) (D.pageId p) req t) $ do
        res <- N.makeResLim reqLim req n
        L.debug l $ show res
        crwlPageRes (D.crwlId c) (D.pageId p) req res t <$> getCurrentTime
    L.debug l $ show cp
    _ <- M.txCrwlPage c cp mCh
    L.info l $ decodeUtf8 (unCrwlHref $
            toRouteHref (D.siteURL s, D.crwlSiteV c)) <> " CRWL " <>
        decodeUtf8 (M.crwlPageReqMethod $ M.crwlPageReq cp) <> " " <>
        show (D.unSiteURL $ D.siteURL s) <>
        show (D.unPageURL $ D.pageURL p) <> " " <>
        showCrwlPageRes (M.crwlPageRes cp)
    L.debug l $ "LIMITING " <> show rateLim <> " μs"
    threadDelay rateLim
    where
        reqLim = 1048576 -- 1 MB
        rateLim = 10000000 -- 10 s


httpH :: L.Logger -> D.CrwlId -> D.PageId -> HTTP.Request -> UTCTime ->
    HTTP.HttpException -> IO M.CrwlPage
httpH l cId pId req t ex = do
    L.err l $ show ex
    return $ crwlPageErr cId pId req (httpExResErr ex) t

httpExResErr :: HTTP.HttpException -> M.CrwlPageResErr
httpExResErr _ = M.Internal

crwlPageErr :: D.CrwlId -> D.PageId -> HTTP.Request -> M.CrwlPageResErr ->
    UTCTime -> M.CrwlPage
crwlPageErr (sId, sV) pId req resErr t = M.CrwlPage {
        M.crwlPageSiteId = sId,
        M.crwlPageSiteV  = sV,
        M.crwlPagePageId = pId,
        M.crwlPagePageV  = D.PageV t,
        M.crwlPageReq    = pageReq,
        M.crwlPageRes    = Left resErr,
        M.crwlPageBlob   = Nothing}
    where
        pageReq = M.CrwlPageReq (HTTP.method req) (HTTP.requestVersion req)

crwlPageRes :: D.CrwlId -> D.PageId -> HTTP.Request ->
    HTTP.Response LByteString -> UTCTime -> UTCTime -> M.CrwlPage
crwlPageRes (sId, sV) pId req res t t' = M.CrwlPage {
        M.crwlPageSiteId = sId,
        M.crwlPageSiteV  = sV,
        M.crwlPagePageId = pId,
        M.crwlPagePageV  = D.PageV t,
        M.crwlPageReq    = pageReq,
        M.crwlPageRes    = Right pageRes,
        M.crwlPageBlob   = Just pageBlob}
    where
        hConvert :: (CI.CI ByteString, ByteString) -> (Text, Text)
        hConvert (k, v) = (decodeUtf8 $ CI.original k, decodeUtf8 v)
        pageReq = M.CrwlPageReq (HTTP.method req) (HTTP.requestVersion req)
        pageRes = M.CrwlPageRes (HTTP.statusCode $ HTTP.responseStatus res)
            (HTTP.responseVersion res) (toRational $ diffUTCTime t' t)
        pageBlob = M.CrwlPageBlob
            (M.fromList (hConvert <$> HTTP.responseHeaders res))
            (HTTP.responseBody res)

showCrwlPageRes :: Show a => Either a M.CrwlPageRes -> Text
showCrwlPageRes (Left e)  = show e
showCrwlPageRes (Right r) = show $ M.crwlPageResStatus r

userAgentStr :: Text -> Text
userAgentStr ver = toText $ R.subRegex r str (toString ver)
    where
        str = "Isoxya/${VERSION} (+https://www.isoxya.com/)"
        r = R.mkRegex "\\$\\{VERSION\\}"
