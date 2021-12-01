{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Isoxya.API.Resource (
    Apex(..),
    Crawl(..),
    CrawlC(..),
    Processor(..),
    ProcessorC(..),
    Site(..),
    SiteC(..),
    Streamer(..),
    StreamerC(..),
    genCrawl,
    genProcessor,
    genSite,
    genStreamer,
    ) where


import           Data.Aeson
import           Data.Time.Clock
import           Isoxya.API.Href
import           Isoxya.URI
import           Network.URI
import           TiredPixel.Common.Snap.CoreUtil
import           TiredPixel.Common.URI
import qualified Isoxya.DB                       as D
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
data Apex = Apex {
    apexNow     :: UTCTime,
    apexVersion :: Text
    } deriving (Show)
instance ToJSON Apex where
    toJSON Apex{..} = object [
        "now"     .= apexNow,
        "version" .= apexVersion]

data Crawl = Crawl {
    crawlHref            :: CrawlHref,
    crawlSite            :: Site,
    crawlStatus          :: D.CrawlStatus,
    crawlPages           :: Maybe Integer,
    crawlProgress        :: Maybe Integer,
    crawlBegan           :: UTCTime,
    crawlEnded           :: Maybe UTCTime,
    crawlProcessorConfig :: Value,
    crawlProcessorHrefs  :: [ProcessorHref],
    crawlStreamerHrefs   :: [StreamerHref]
    } deriving (Show)
instance ToJSON Crawl where
    toJSON Crawl{..} = object [
        "href"             .= crawlHref,
        "site"             .= crawlSite,
        "status"           .= crawlStatus,
        "pages"            .= crawlPages,
        "progress"         .= crawlProgress,
        "began"            .= crawlBegan,
        "ended"            .= crawlEnded,
        "processor_config" .= crawlProcessorConfig,
        "processors"       .= map (objHref . Just) crawlProcessorHrefs,
        "streamers"        .= map (objHref . Just) crawlStreamerHrefs]

data CrawlC = CrawlC {
    crawlCProcessorConfig :: Value,
    crawlCProcessorHrefs  :: [ProcessorHref],
    crawlCStreamerHrefs   :: [StreamerHref]
    } deriving (Show)
instance FromJSON CrawlC where
    parseJSON = withObject "crawl" $ \j -> do
        fProcessors <- j .: "processors"
        fStreamers  <- j .: "streamers"
        crawlCProcessorConfig <- j .:? "processor_config" .!= Null
        crawlCProcessorHrefs  <- mapM (.: "href") fProcessors
        crawlCStreamerHrefs   <- mapM (.: "href") fStreamers
        return $ CrawlC{..}
instance ValidateJSON CrawlC

data Processor = Processor {
    processorHref :: ProcessorHref,
    processorURL  :: URI,
    processorTag  :: Text
    } deriving (Show)
instance ToJSON Processor where
    toJSON Processor{..} = object [
        "href" .= processorHref,
        "url"  .= processorURL,
        "tag"  .= processorTag]

data ProcessorC = ProcessorC {
    processorCURL :: URIAbsolute,
    processorCTag :: Text
    } deriving (Show)
instance FromJSON ProcessorC where
    parseJSON = withObject "processor" $ \j -> ProcessorC <$>
        j .: "url" <*>
        j .: "tag"
instance ValidateJSON ProcessorC

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

data Streamer = Streamer {
    streamerHref :: StreamerHref,
    streamerURL  :: URI,
    streamerTag  :: Text
    } deriving (Show)
instance ToJSON Streamer where
    toJSON Streamer{..} = object [
        "href" .= streamerHref,
        "url"  .= streamerURL,
        "tag"  .= streamerTag]

data StreamerC = StreamerC {
    streamerCURL :: URIAbsolute,
    streamerCTag :: Text
    } deriving (Show)
instance FromJSON StreamerC where
    parseJSON = withObject "streamer" $ \j -> StreamerC <$>
        j .: "url" <*>
        j .: "tag"
instance ValidateJSON StreamerC

genCrawl :: D.Site -> D.Crawl -> Crawl
genCrawl s c = Crawl {
    crawlHref             = toRouteHref (D.siteURL s, D.crawlSiteV c),
    crawlSite             = genSite s,
    crawlStatus           = D.crawlStatus c,
    crawlPages            = D.crawlPages c,
    crawlProgress         = D.crawlProgress c,
    crawlBegan            = D.unSiteV $ D.crawlSiteV c,
    crawlEnded            = D.crawlEnded c,
    crawlProcessorConfig  = D.crawlProcessorConf c,
    crawlProcessorHrefs   = map toRouteHref $ D.crawlProcessorIds c,
    crawlStreamerHrefs    = map toRouteHref $ D.crawlStreamerIds c}

genProcessor :: D.Processor -> Processor
genProcessor p = Processor {
    processorHref = toRouteHref $ D.processorId p,
    processorURL  = D.unProcessorURL $ D.processorURL p,
    processorTag  = D.processorTag p}

genSite :: D.Site -> Site
genSite s = Site {
    siteHref = toRouteHref $ D.siteURL s,
    siteURL  = D.unSiteURL $ D.siteURL s}

genStreamer :: D.Streamer -> Streamer
genStreamer s = Streamer {
    streamerHref = toRouteHref $ D.streamerId s,
    streamerURL  = D.unStreamerURL $ D.streamerURL s,
    streamerTag  = D.streamerTag s}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
objHref :: ToJSON v => Maybe v -> Value
objHref (Just v) = object ["href" .= v]
objHref Nothing  = Null
--------------------------------------------------------------------------------
instance FromJSON D.CrawlStatus where
    parseJSON = withText "CrawlStatus" $ \case
        "pending"   -> pure D.CrawlStatusPending
        "completed" -> pure D.CrawlStatusCompleted
        "limited"   -> pure D.CrawlStatusLimited
        "canceled"  -> pure D.CrawlStatusCanceled
        _           -> fail "invalid CrawlStatus"
instance ToJSON D.CrawlStatus where
    toJSON v = toJSON ((case v of
        D.CrawlStatusPending   -> "pending"
        D.CrawlStatusCompleted -> "completed"
        D.CrawlStatusLimited   -> "limited"
        D.CrawlStatusCanceled  -> "canceled") :: Text)
