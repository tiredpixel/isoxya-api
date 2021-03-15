{-# LANGUAGE RecordWildCards #-}


module ISX.CE.API.Resource (
    Apex(..),
    Site(..),
    SiteC(..),
    site,
    ) where


import           Data.Aeson
import           Data.Time.Clock
import           ISX.CE.API.Href
import           ISX.CE.URI
import           Network.URI
import           TPX.Com.Snap.CoreUtils
import           TPX.Com.URI
import qualified ISX.CE.DB              as D
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
data Apex = Apex {
    apexTNow    :: UTCTime,
    apexVersion :: Text
    } deriving (Show)
instance ToJSON Apex where
    toJSON Apex{..} = object [
        "t_now"   .= apexTNow,
        "version" .= apexVersion]

data Site = Site {
    siteHref :: SiteHref,
    siteURL  :: URI
    } deriving (Show)
instance ToJSON Site where
    toJSON Site{..} = object [
        "href" .= siteHref,
        "url"  .= siteURL]

newtype SiteC = SiteC {
    siteCURL :: URISite
    } deriving (Show)
instance FromJSON SiteC where
    parseJSON = withObject "site" $ \j -> SiteC <$>
        j .: "url"
instance ValidateJSON SiteC

site :: D.Site -> Site
site s = Site {
    siteHref = toRouteHref $ D.siteURL s,
    siteURL  = D.unSiteURL $ D.siteURL s}
