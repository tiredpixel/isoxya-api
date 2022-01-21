{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Isoxya.API.Href (
    CrawlHref(..),
    CrawlsHref(..),
    ProcessorHref(..),
    ProcessorsHref(..),
    SiteHref(..),
    StreamerHref(..),
    StreamersHref(..),
    ) where


import           Data.Aeson
import           Network.URI
import           TiredPixel.Common.Snap.CoreUtil
import qualified Data.ByteString.Base64.URL      as B64
import qualified Data.ByteString.Char8           as C8
import qualified Data.Time.Format                as Time
import qualified Data.UUID                       as UUID
import qualified Isoxya.DB                       as D
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
newtype CrawlHref = CrawlHref { unCrawlHref :: ByteString
    } deriving (Show)
instance RouteHref CrawlHref (D.SiteURL, D.SiteV) where
    toRouteHref (s, c) = CrawlHref $
        unSiteHref (toRouteHref s) <> crawls <> "/" <> fromRouteId c
    fromRouteHref h = do
        ["", "site", s, "crawl", c] <- return $ C8.split '/' $ unCrawlHref h
        s' <- toRouteId s
        c' <- toRouteId c
        return (s', c')
instance ToJSON CrawlHref where
    toJSON o = toJSON (decodeUtf8 $ unCrawlHref o :: Text)

newtype CrawlsHref = CrawlsHref { unCrawlsHref :: ByteString
    } deriving (Show)
instance RouteHref CrawlsHref D.SiteURL where
    toRouteHref s = CrawlsHref $ unSiteHref (toRouteHref s) <> crawls
    fromRouteHref h = do
        ["", "site", s, "crawl"] <- return $ C8.split '/' $ unCrawlsHref h
        toRouteId s
instance ToJSON CrawlsHref where
    toJSON o = toJSON (decodeUtf8 $ unCrawlsHref o :: Text)

newtype ProcessorHref = ProcessorHref { unProcessorHref :: ByteString
    } deriving (Show)
instance RouteHref ProcessorHref D.ProcessorId where
    toRouteHref p = ProcessorHref $ processors <> "/" <> fromRouteId p
    fromRouteHref h = do
        ["", "processor", p] <- return $ C8.split '/' $ unProcessorHref h
        toRouteId p
instance FromJSON ProcessorHref where
    parseJSON = withText "processor.href" $ pure . ProcessorHref . encodeUtf8
instance ToJSON ProcessorHref where
    toJSON o = toJSON (decodeUtf8 $ unProcessorHref o :: Text)

newtype ProcessorsHref = ProcessorsHref { unProcessorsHref :: ByteString
    } deriving (Show)
instance RouteHref ProcessorsHref () where
    toRouteHref _ = ProcessorsHref processors
    fromRouteHref h = do
        ["", "processor"] <- return $ C8.split '/' $ unProcessorsHref h
        pass
instance ToJSON ProcessorsHref where
    toJSON o = toJSON (decodeUtf8 $ unProcessorsHref o :: Text)

newtype SiteHref = SiteHref { unSiteHref :: ByteString
    } deriving (Show)
instance RouteHref SiteHref D.SiteURL where
    toRouteHref s = SiteHref $ sites <> "/" <> fromRouteId s
    fromRouteHref h = do
        ["", "site", s] <- return $ C8.split '/' $ unSiteHref h
        toRouteId s
instance ToJSON SiteHref where
    toJSON o = toJSON (decodeUtf8 $ unSiteHref o :: Text)

newtype StreamerHref = StreamerHref { unStreamerHref :: ByteString
    } deriving (Show)
instance RouteHref StreamerHref D.StreamerId where
    toRouteHref s = StreamerHref $ streamers <> "/" <> fromRouteId s
    fromRouteHref h = do
        ["", "streamer", s] <- return $ C8.split '/' $ unStreamerHref h
        toRouteId s
instance FromJSON StreamerHref where
    parseJSON = withText "streamer.href" $ pure . StreamerHref . encodeUtf8
instance ToJSON StreamerHref where
    toJSON o = toJSON (decodeUtf8 $ unStreamerHref o :: Text)

newtype StreamersHref = StreamersHref { unStreamersHref :: ByteString
    } deriving (Show)
instance RouteHref StreamersHref () where
    toRouteHref _ = StreamersHref streamers
    fromRouteHref h = do
        ["", "streamer"] <- return $ C8.split '/' $ unStreamersHref h
        pass
instance ToJSON StreamersHref where
    toJSON o = toJSON (decodeUtf8 $ unStreamersHref o :: Text)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
crawls :: ByteString
crawls = "/crawl"

processors :: ByteString
processors = "/processor"

sites :: ByteString
sites = "/site"

streamers :: ByteString
streamers = "/streamer"
--------------------------------------------------------------------------------
timeF :: String
timeF = Time.iso8601DateFormat (Just "%T%QZ")
--------------------------------------------------------------------------------
instance RouteId D.ProcessorId where
    toRouteId b = D.ProcessorId <$> UUID.fromASCIIBytes b
    fromRouteId = show . D.unProcessorId

instance RouteId D.SiteURL where
    toRouteId b = D.SiteURL <$>
        parseAbsoluteURI (decodeUtf8 $ B64.decodeLenient b)
    fromRouteId = B64.encodeUnpadded . show . D.unSiteURL

instance RouteId D.SiteV where
    toRouteId b = D.SiteV <$>
        Time.parseTimeM False Time.defaultTimeLocale timeF (decodeUtf8 b)
    fromRouteId = encodeUtf8 .
        Time.formatTime Time.defaultTimeLocale timeF . D.unSiteV

instance RouteId D.StreamerId where
    toRouteId b = D.StreamerId <$> UUID.fromASCIIBytes b
    fromRouteId = show . D.unStreamerId
