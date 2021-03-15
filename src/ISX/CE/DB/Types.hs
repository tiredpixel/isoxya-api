module ISX.CE.DB.Types (
    Site(..),
    SiteId(..),
    SiteURL(..),
    ) where


import           Data.Time.Clock
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField
import           Network.URI
import           TPX.Com.SQLite.Ext.Hash        ()
import           TPX.Com.SQLite.Ext.URI         ()
import qualified Crypto.Hash                    as Hash


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
