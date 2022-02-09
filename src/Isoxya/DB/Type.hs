module Isoxya.DB.Type (
    Crawl(..),
    CrawlId ,
    CrawlId'(..),
    CrawlStatus(..),
    Page(..),
    PageId(..),
    PageURL(..),
    PageV(..),
    Processor(..),
    ProcessorId(..),
    ProcessorURL(..),
    Site(..),
    SiteId(..),
    SiteURL(..),
    SiteV(..),
    Streamer(..),
    StreamerId(..),
    StreamerURL(..),
    crawlId,
    ) where


import qualified Crypto.Hash                       as Hash
import qualified Data.Aeson                        as A
import qualified Data.ByteString.Char8             as C8
import qualified Data.Text                         as T
import           Data.Time.Clock
import           Data.UUID                         hiding (toString, toText)
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.ToField
import           Database.SQLite3
import           Network.URI
import           TiredPixel.Common.SQLite.Ext.Hash ()
import           TiredPixel.Common.SQLite.Ext.URI  ()
import           TiredPixel.Common.SQLite.Ext.UUID ()


data Crawl = Crawl
               { crawlSiteId          :: SiteId
               , crawlSiteV           :: SiteV
               , crawlStatus          :: CrawlStatus
               , crawlPages           :: Maybe Integer
               , crawlProcessed       :: Maybe Integer
               , crawlProgress        :: Maybe Integer
               , crawlEnded           :: Maybe UTCTime
               , crawlProcessorConfig :: A.Value
               , crawlProcessorIds    :: [ProcessorId]
               , crawlStreamerIds     :: [StreamerId]
               }
  deriving (Show)
instance FromRow Crawl where
    fromRow = Crawl <$>
        (SiteId <$> field) <*>
        (SiteV <$> field) <*>
        field <*>
        field <*>
        field <*>
        field <*>
        field <*>
        (fromMaybe A.Null . A.decode <$> field) <*>
        (map ProcessorId . mapMaybe fromASCIIBytes . C8.split ',' <$> field) <*>
        (map StreamerId . mapMaybe fromASCIIBytes . C8.split ',' <$> field)

type CrawlId = (SiteId, SiteV)

data CrawlId' = CrawlId'
                  { crawlSiteId' :: SiteId
                  , crawlSiteV'  :: SiteV
                  }
  deriving (Show)
instance FromRow CrawlId' where
    fromRow = CrawlId' <$>
        (SiteId <$> field) <*>
        (SiteV <$> field)

data CrawlStatus = CrawlStatusPending | CrawlStatusCompleted | CrawlStatusLimited | CrawlStatusCanceled deriving
    ( Eq
    , Show
    )
instance FromField CrawlStatus where
    fromField f@(Field (SQLText d) _) = case d of
        "Pending"   -> return CrawlStatusPending
        "Completed" -> return CrawlStatusCompleted
        "Limited"   -> return CrawlStatusLimited
        "Canceled"  -> return CrawlStatusCanceled
        _           -> returnError ConversionFailed f "must be CrawlStatus"
    fromField f = returnError ConversionFailed f "must be SQLText"
instance ToField CrawlStatus where
    toField e = toField (T.drop 11 $ show e :: Text)

data Page = Page
              { pageId     :: PageId
              , pageSiteId :: SiteId
              , pageURL    :: PageURL
              }
  deriving (Show)
instance FromRow Page where
    fromRow = Page <$>
        (PageId <$> field) <*>
        (SiteId <$> field) <*>
        (PageURL <$> field)

newtype PageId = PageId { unPageId :: Hash.Digest Hash.SHA256 }
  deriving (Show)
instance FromRow PageId where
    fromRow = PageId <$> field
instance ToField PageId where
    toField = toField . unPageId

newtype PageURL = PageURL { unPageURL :: URI }
  deriving (Show)
instance FromRow PageURL where
    fromRow = PageURL <$> field
instance ToField PageURL where
    toField = toField . unPageURL

newtype PageV = PageV { unPageV :: UTCTime }
  deriving (Show)
instance ToField PageV where
    toField = toField . unPageV

data Processor = Processor
                   { processorId       :: ProcessorId
                   , processorURL      :: ProcessorURL
                   , processorTag      :: Text
                   , processorInserted :: UTCTime
                   }
  deriving (Show)
instance FromRow Processor where
    fromRow = Processor <$>
        (ProcessorId <$> field) <*>
        (ProcessorURL <$> field) <*>
        field <*>
        field

newtype ProcessorId = ProcessorId { unProcessorId :: UUID }
  deriving (Show)
instance FromRow ProcessorId where
    fromRow = ProcessorId <$> field
instance ToField ProcessorId where
    toField = toField . unProcessorId

newtype ProcessorURL = ProcessorURL { unProcessorURL :: URI }
  deriving (Show)
instance FromRow ProcessorURL where
    fromRow = ProcessorURL <$> field
instance ToField ProcessorURL where
    toField = toField . unProcessorURL

data Site = Site
              { siteId       :: SiteId
              , siteURL      :: SiteURL
              , siteInserted :: UTCTime
              }
  deriving (Show)
instance FromRow Site where
    fromRow = Site <$>
        (SiteId  <$> field) <*>
        (SiteURL <$> field) <*>
        field

newtype SiteId = SiteId { unSiteId :: Hash.Digest Hash.SHA256 }
  deriving (Eq, Show)
instance FromRow SiteId where
    fromRow = SiteId <$> field
instance ToField SiteId where
    toField = toField . unSiteId

newtype SiteURL = SiteURL { unSiteURL :: URI }
  deriving (Show)
instance FromRow SiteURL where
    fromRow = SiteURL <$> field
instance ToField SiteURL where
    toField = toField . unSiteURL

newtype SiteV = SiteV { unSiteV :: UTCTime }
  deriving (Show)
instance FromRow SiteV where
    fromRow = SiteV <$> field
instance ToField SiteV where
    toField = toField . unSiteV

data Streamer = Streamer
                  { streamerId       :: StreamerId
                  , streamerURL      :: StreamerURL
                  , streamerTag      :: Text
                  , streamerInserted :: UTCTime
                  }
  deriving (Show)
instance FromRow Streamer where
    fromRow = Streamer <$>
        (StreamerId <$> field) <*>
        (StreamerURL <$> field) <*>
        field <*>
        field

newtype StreamerId = StreamerId { unStreamerId :: UUID }
  deriving (Show)
instance FromRow StreamerId where
    fromRow = StreamerId <$> field
instance ToField StreamerId where
    toField = toField . unStreamerId

newtype StreamerURL = StreamerURL { unStreamerURL :: URI }
  deriving (Show)
instance FromRow StreamerURL where
    fromRow = StreamerURL <$> field
instance ToField StreamerURL where
    toField = toField . unStreamerURL

crawlId :: Crawl -> CrawlId
crawlId crl = (crawlSiteId crl, crawlSiteV crl)
