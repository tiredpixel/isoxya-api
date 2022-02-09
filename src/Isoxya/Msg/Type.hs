module Isoxya.Msg.Type (
    ChanCrawler,
    ChanProcessor,
    ChanStreamer,
    CrawlPage(..),
    CrawlPageBlob(..),
    CrawlPageData(..),
    CrawlPageId(..),
    CrawlPageRequest(..),
    CrawlPageResponse(..),
    CrawlPageResponseError(..),
    MsgCrawler,
    MsgProcessor,
    MsgStreamer,
    httpExResponseError,
    showCrawlPageResponse,
    ) where


import           Control.Concurrent.Chan
import qualified Data.Aeson              as A
import qualified Isoxya.DB               as D
import qualified Network.HTTP.Conduit    as HTTP
import qualified Network.HTTP.Types      as HTTP


type ChanCrawler = Chan MsgCrawler

type ChanProcessor = Chan MsgProcessor

type ChanStreamer = Chan MsgStreamer

data CrawlPage = CrawlPage
                   { crawlPageSiteId :: D.SiteId
                   , crawlPageSiteV :: D.SiteV
                   , crawlPagePageId :: D.PageId
                   , crawlPagePageV :: D.PageV
                   , crawlPageRequest :: CrawlPageRequest
                   , crawlPageResponse :: Either CrawlPageResponseError CrawlPageResponse
                   , crawlPageBlob :: Maybe CrawlPageBlob
                   }
  deriving (Show)

data CrawlPageBlob = CrawlPageBlob
                       { crawlPageBlobHeader :: Map Text Text
                       , crawlPageBlobBody   :: LByteString
                       }
  deriving (Show)

data CrawlPageData = CrawlPageData
                       { crawlPageDataSiteId      :: D.SiteId
                       , crawlPageDataSiteV       :: D.SiteV
                       , crawlPageDataPageId      :: D.PageId
                       , crawlPageDataPageV       :: D.PageV
                       , crawlPageDataProcessorId :: D.ProcessorId
                       , crawlPageDataData        :: A.Value
                       }
  deriving (Show)

data CrawlPageId = CrawlPageId
                     { crawlPageIdSiteId :: D.SiteId
                     , crawlPageIdSiteV  :: D.SiteV
                     , crawlPageIdPageId :: D.PageId
                     }
  deriving (Show)

data CrawlPageRequest = CrawlPageRequest
                          { crawlPageRequestMethod  :: HTTP.Method
                          , crawlPageRequestVersion :: HTTP.HttpVersion
                          }
  deriving (Show)

data CrawlPageResponse = CrawlPageResponse
                           { crawlPageResponseStatus   :: Int
                           , crawlPageResponseVersion  :: HTTP.HttpVersion
                           , crawlPageResponseDuration :: Rational
                           }
  deriving (Show)

data CrawlPageResponseError = Internal deriving (Show)

type MsgCrawler = (D.SiteId, CrawlPageId)

type MsgProcessor = (D.ProcessorId, CrawlPage)

type MsgStreamer = (D.StreamerId, CrawlPageData)

httpExResponseError :: HTTP.HttpException -> CrawlPageResponseError
httpExResponseError _ = Internal

showCrawlPageResponse :: Show a => Either a CrawlPageResponse -> Text
showCrawlPageResponse (Left e)  = show e
showCrawlPageResponse (Right r) = show $ crawlPageResponseStatus r
