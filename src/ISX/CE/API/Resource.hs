{-# LANGUAGE RecordWildCards #-}


module ISX.CE.API.Resource (
    Apex(..),
    PlugProc(..),
    PlugProcC(..),
    Site(..),
    SiteC(..),
    plugProc,
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

data PlugProc = PlugProc {
    plugProcHref :: PlugProcHref,
    plugProcURL  :: URI,
    plugProcTag  :: Text
    } deriving (Show)
instance ToJSON PlugProc where
    toJSON PlugProc{..} = object [
        "href" .= plugProcHref,
        "url"  .= plugProcURL,
        "tag"  .= plugProcTag]

data PlugProcC = PlugProcC {
    plugProcCURL :: URIAbsolute,
    plugProcCTag :: Text
    } deriving (Show)
instance FromJSON PlugProcC where
    parseJSON = withObject "plug_proc" $ \j -> PlugProcC <$>
        j .: "url" <*>
        j .: "tag"
instance ValidateJSON PlugProcC

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

plugProc :: D.PlugProc -> PlugProc
plugProc pp = PlugProc {
    plugProcHref = toRouteHref $ D.plugProcId pp,
    plugProcURL  = D.unPlugProcURL $ D.plugProcURL pp,
    plugProcTag  = D.plugProcTag pp}

site :: D.Site -> Site
site s = Site {
    siteHref = toRouteHref $ D.siteURL s,
    siteURL  = D.unSiteURL $ D.siteURL s}
