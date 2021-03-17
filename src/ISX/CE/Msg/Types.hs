module ISX.CE.Msg.Types (
    ChanCrwl,
    ChanProc,
    CrwlPage(..),
    CrwlPageBlob(..),
    CrwlPageId(..),
    CrwlPageReq(..),
    CrwlPageRes(..),
    CrwlPageResErr(..),
    MsgCrwl,
    MsgProc,
    httpExResErr,
    showCrwlPageRes,
    ) where


import           Control.Concurrent.Chan
import qualified ISX.CE.DB               as D
import qualified Network.HTTP.Conduit    as HTTP
import qualified Network.HTTP.Types      as HTTP


type ChanCrwl = Chan MsgCrwl

type ChanProc = Chan MsgProc

data CrwlPage = CrwlPage {
    crwlPageSiteId :: D.SiteId,
    crwlPageSiteV  :: D.SiteV,
    crwlPagePageId :: D.PageId,
    crwlPagePageV  :: D.PageV,
    crwlPageReq    :: CrwlPageReq,
    crwlPageRes    :: Either CrwlPageResErr CrwlPageRes,
    crwlPageBlob   :: Maybe CrwlPageBlob
    } deriving (Show)

data CrwlPageBlob = CrwlPageBlob {
    crwlPageBlobHeader :: Map Text Text,
    crwlPageBlobBody   :: LByteString
    } deriving (Show)

data CrwlPageId = CrwlPageId {
    crwlPageIdSiteId :: D.SiteId,
    crwlPageIdSiteV  :: D.SiteV,
    crwlPageIdPageId :: D.PageId
    } deriving (Show)

data CrwlPageReq = CrwlPageReq {
    crwlPageReqMethod  :: HTTP.Method,
    crwlPageReqVersion :: HTTP.HttpVersion
    } deriving (Show)

data CrwlPageRes = CrwlPageRes {
    crwlPageResStatus   :: Int,
    crwlPageResVersion  :: HTTP.HttpVersion,
    crwlPageResDuration :: Rational
    } deriving (Show)

data CrwlPageResErr =
    Internal
    deriving (Show)

type MsgCrwl = (D.SiteId, CrwlPageId)

type MsgProc = (D.PlugProcId, CrwlPage)

httpExResErr :: HTTP.HttpException -> CrwlPageResErr
httpExResErr _ = Internal

showCrwlPageRes :: Show a => Either a CrwlPageRes -> Text
showCrwlPageRes (Left e)  = show e
showCrwlPageRes (Right r) = show $ crwlPageResStatus r
