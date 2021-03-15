{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module ISX.CE.API.Resource (
    Apex(..),
    Crwl(..),
    CrwlC(..),
    PlugProc(..),
    PlugProcC(..),
    PlugStrm(..),
    PlugStrmC(..),
    Site(..),
    SiteC(..),
    crwl,
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

data Crwl = Crwl {
    crwlHref          :: CrwlHref,
    crwlStatus        :: D.CrwlStatus,
    crwlPages         :: Maybe Integer,
    crwlProgress      :: Maybe Integer,
    crwlTBegin        :: UTCTime,
    crwlTEnd          :: Maybe UTCTime,
    crwlSite          :: Site,
    crwlPlugProcConf  :: Value,
    crwlPlugProcHrefs :: [PlugProcHref],
    crwlPlugStrmHrefs :: [PlugStrmHref]
    } deriving (Show)
instance ToJSON Crwl where
    toJSON Crwl{..} = object [
        "href"           .= crwlHref,
        "status"         .= crwlStatus,
        "pages"          .= crwlPages,
        "progress"       .= crwlProgress,
        "t_begin"        .= crwlTBegin,
        "t_end"          .= crwlTEnd,
        "site"           .= crwlSite,
        "plug_proc_conf" .= crwlPlugProcConf,
        "plug_proc"      .= map (objHref . Just) crwlPlugProcHrefs,
        "plug_strm"      .= map (objHref . Just) crwlPlugStrmHrefs]

data CrwlC = CrwlC {
    crwlCPlugProcConf  :: Value,
    crwlCPlugProcHrefs :: [PlugProcHref],
    crwlCPlugStrmHrefs :: [PlugStrmHref]
    } deriving (Show)
instance FromJSON CrwlC where
    parseJSON = withObject "crwl" $ \j -> do
        crwlCPlugProcConf <- j .:? "plug_proc_conf" .!= Null
        fPlugProcs <- j .: "plug_proc"
        crwlCPlugProcHrefs <- mapM (.: "href") fPlugProcs
        fPlugStrms <- j .: "plug_strm"
        crwlCPlugStrmHrefs <- mapM (.: "href") fPlugStrms
        return $ CrwlC{..}
instance ValidateJSON CrwlC

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

crwl :: D.Site -> D.Crwl -> Crwl
crwl s c = Crwl {
    crwlHref          = toRouteHref (D.siteURL s, D.crwlSiteV c),
    crwlStatus        = D.crwlStatus c,
    crwlPages         = D.crwlPages c,
    crwlProgress      = D.crwlProgress c,
    crwlTBegin        = D.unSiteV $ D.crwlSiteV c,
    crwlTEnd          = D.crwlTEnd c,
    crwlSite          = site s,
    crwlPlugProcConf  = D.crwlPlugProcConf c,
    crwlPlugProcHrefs = map toRouteHref $ D.crwlPlugProcIds c,
    crwlPlugStrmHrefs = map toRouteHref $ D.crwlPlugStrmIds c}

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
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
objHref :: ToJSON v => Maybe v -> Value
objHref (Just v) = object ["href" .= v]
objHref Nothing  = Null
--------------------------------------------------------------------------------
instance FromJSON D.CrwlStatus where
    parseJSON = withText "CrwlStatus" $ \case
        "pending"   -> pure D.CrwlStatusPending
        "completed" -> pure D.CrwlStatusCompleted
        "limited"   -> pure D.CrwlStatusLimited
        "canceled"  -> pure D.CrwlStatusCanceled
        _ -> fail "Invalid CrwlStatus"
instance ToJSON D.CrwlStatus where
    toJSON v = toJSON ((case v of
        D.CrwlStatusPending   -> "pending"
        D.CrwlStatusCompleted -> "completed"
        D.CrwlStatusLimited   -> "limited"
        D.CrwlStatusCanceled  -> "canceled") :: Text)
