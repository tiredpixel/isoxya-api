module ISX.CE.DB.Types (
    Crwl(..),
    CrwlId ,
    CrwlId'(..),
    CrwlStatus(..),
    Page(..),
    PageId(..),
    PageURL(..),
    PlugProc(..),
    PlugProcId(..),
    PlugProcURL(..),
    PlugStrm(..),
    PlugStrmId(..),
    PlugStrmURL(..),
    Site(..),
    SiteId(..),
    SiteURL(..),
    SiteV(..),
    crwlId,
    ) where


import           Data.Time.Clock
import           Data.UUID                        hiding (toString, toText)
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.ToField
import           Database.SQLite3
import           Network.URI
import           TPX.Com.SQLite.Ext.Hash          ()
import           TPX.Com.SQLite.Ext.URI           ()
import           TPX.Com.SQLite.Ext.UUID          ()
import qualified Crypto.Hash                      as Hash
import qualified Data.Aeson                       as A
import qualified Data.ByteString.Char8            as C8
import qualified Data.Text                        as T


data Crwl = Crwl {
    crwlSiteId       :: SiteId,
    crwlSiteV        :: SiteV,
    crwlStatus       :: CrwlStatus,
    crwlPages        :: Maybe Integer,
    crwlProcessed    :: Maybe Integer,
    crwlProgress     :: Maybe Integer,
    crwlTEnd         :: Maybe UTCTime,
    crwlPlugProcConf :: A.Value,
    crwlPlugProcIds  :: [PlugProcId],
    crwlPlugStrmIds  :: [PlugStrmId]
    } deriving (Show)
instance FromRow Crwl where
    fromRow = Crwl <$>
        (SiteId <$> field) <*>
        (SiteV <$> field) <*>
        field <*>
        field <*>
        field <*>
        field <*>
        field <*>
        (fromMaybe A.Null . A.decode <$> field) <*>
        (map PlugProcId . mapMaybe fromASCIIBytes . C8.split ',' <$> field) <*>
        (map PlugStrmId . mapMaybe fromASCIIBytes . C8.split ',' <$> field)

type CrwlId = (SiteId, SiteV)

data CrwlId' = CrwlId' {
    crwlSiteId' :: SiteId,
    crwlSiteV'  :: SiteV
    } deriving (Show)
instance FromRow CrwlId' where
    fromRow = CrwlId' <$>
        (SiteId <$> field) <*>
        (SiteV <$> field)

data CrwlStatus =
    CrwlStatusPending |
    CrwlStatusCompleted |
    CrwlStatusLimited |
    CrwlStatusCanceled
    deriving (Show, Eq)
instance FromField CrwlStatus where
    fromField f@(Field (SQLText d) _) = case d of
        "Pending"   -> return CrwlStatusPending
        "Completed" -> return CrwlStatusCompleted
        "Limited"   -> return CrwlStatusLimited
        "Canceled"  -> return CrwlStatusCanceled
        _ -> returnError ConversionFailed f "must be CrwlStatus"
    fromField f = returnError ConversionFailed f "must be SQLText"
instance ToField CrwlStatus where
    toField e = toField (T.drop 10 $ show e :: Text)

data Page = Page {
    pageId     :: PageId,
    pageSiteId :: SiteId,
    pageURL    :: PageURL
    } deriving (Show)
instance FromRow Page where
    fromRow = Page <$>
        (PageId <$> field) <*>
        (SiteId <$> field) <*>
        (PageURL <$> field)

newtype PageId = PageId { unPageId :: Hash.Digest Hash.SHA256
    } deriving (Show)
instance FromRow PageId where
    fromRow = PageId <$> field
instance ToField PageId where
    toField = toField . unPageId

newtype PageURL = PageURL { unPageURL :: URI
    } deriving (Show)
instance FromRow PageURL where
    fromRow = PageURL <$> field
instance ToField PageURL where
    toField = toField . unPageURL

data PlugProc = PlugProc {
    plugProcId   :: PlugProcId,
    plugProcURL  :: PlugProcURL,
    plugProcTag  :: Text,
    plugProcTIns :: UTCTime
    } deriving (Show)
instance FromRow PlugProc where
    fromRow = PlugProc <$>
        (PlugProcId <$> field) <*>
        (PlugProcURL <$> field) <*>
        field <*>
        field

newtype PlugProcId = PlugProcId { unPlugProcId :: UUID
    } deriving (Show)
instance FromRow PlugProcId where
    fromRow = PlugProcId <$> field
instance ToField PlugProcId where
    toField = toField . unPlugProcId

newtype PlugProcURL = PlugProcURL { unPlugProcURL :: URI
    } deriving (Show)
instance FromRow PlugProcURL where
    fromRow = PlugProcURL <$> field
instance ToField PlugProcURL where
    toField = toField . unPlugProcURL

data PlugStrm = PlugStrm {
    plugStrmId   :: PlugStrmId,
    plugStrmURL  :: PlugStrmURL,
    plugStrmTag  :: Text,
    plugStrmTIns :: UTCTime
    } deriving (Show)
instance FromRow PlugStrm where
    fromRow = PlugStrm <$>
        (PlugStrmId <$> field) <*>
        (PlugStrmURL <$> field) <*>
        field <*>
        field

newtype PlugStrmId = PlugStrmId { unPlugStrmId :: UUID
    } deriving (Show)
instance FromRow PlugStrmId where
    fromRow = PlugStrmId <$> field
instance ToField PlugStrmId where
    toField = toField . unPlugStrmId

newtype PlugStrmURL = PlugStrmURL { unPlugStrmURL :: URI
    } deriving (Show)
instance FromRow PlugStrmURL where
    fromRow = PlugStrmURL <$> field
instance ToField PlugStrmURL where
    toField = toField . unPlugStrmURL

data Site = Site {
    siteId   :: SiteId,
    siteURL  :: SiteURL,
    siteTIns :: UTCTime
    } deriving (Show)
instance FromRow Site where
    fromRow = Site <$>
        (SiteId  <$> field) <*>
        (SiteURL <$> field) <*>
        field

newtype SiteId = SiteId { unSiteId :: Hash.Digest Hash.SHA256
    } deriving (Show, Eq)
instance FromRow SiteId where
    fromRow = SiteId <$> field
instance ToField SiteId where
    toField = toField . unSiteId

newtype SiteURL = SiteURL { unSiteURL :: URI
    } deriving (Show)
instance FromRow SiteURL where
    fromRow = SiteURL <$> field
instance ToField SiteURL where
    toField = toField . unSiteURL

newtype SiteV = SiteV { unSiteV :: UTCTime
    } deriving (Show)
instance FromRow SiteV where
    fromRow = SiteV <$> field
instance ToField SiteV where
    toField = toField . unSiteV

crwlId :: Crwl -> CrwlId
crwlId c = (crwlSiteId c, crwlSiteV c)
