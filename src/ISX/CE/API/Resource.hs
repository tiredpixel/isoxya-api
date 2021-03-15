{-# LANGUAGE RecordWildCards #-}


module ISX.CE.API.Resource (
    Apex(..),
    PlugProc(..),
    PlugProcC(..),
    PlugStrm(..),
    PlugStrmC(..),
    Site(..),
    SiteC(..),
    plugProc,
    plugStrm,
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

data PlugStrm = PlugStrm {
    plugStrmHref :: PlugStrmHref,
    plugStrmURL  :: URI,
    plugStrmTag  :: Text
    } deriving (Show)
instance ToJSON PlugStrm where
    toJSON PlugStrm{..} = object [
        "href" .= plugStrmHref,
        "url"  .= plugStrmURL,
        "tag"  .= plugStrmTag]

data PlugStrmC = PlugStrmC {
    plugStrmCURL :: URIAbsolute,
    plugStrmCTag :: Text
    } deriving (Show)
instance FromJSON PlugStrmC where
    parseJSON = withObject "plug_strm" $ \j -> PlugStrmC <$>
        j .: "url" <*>
        j .: "tag"
instance ValidateJSON PlugStrmC

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

plugStrm :: D.PlugStrm -> PlugStrm
plugStrm ps = PlugStrm {
    plugStrmHref = toRouteHref $ D.plugStrmId ps,
    plugStrmURL  = D.unPlugStrmURL $ D.plugStrmURL ps,
    plugStrmTag  = D.plugStrmTag ps}

site :: D.Site -> Site
site s = Site {
    siteHref = toRouteHref $ D.siteURL s,
    siteURL  = D.unSiteURL $ D.siteURL s}
