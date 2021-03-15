module ISX.CE.DB.Types (
    PlugProc(..),
    PlugProcId(..),
    PlugProcURL(..),
    PlugStrm(..),
    PlugStrmId(..),
    PlugStrmURL(..),
    Site(..),
    SiteId(..),
    SiteURL(..),
    ) where


import           Data.Time.Clock
import           Data.UUID                      hiding (toString, toText)
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField
import           Network.URI
import           TPX.Com.SQLite.Ext.Hash        ()
import           TPX.Com.SQLite.Ext.URI         ()
import           TPX.Com.SQLite.Ext.UUID        ()
import qualified Crypto.Hash                    as Hash


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
