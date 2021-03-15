{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module ISX.CE.API.Href (
    SiteHref(..),
    SitesHref(..),
    ) where


import           Data.Aeson
import           Network.URI
import           TPX.Com.Snap.CoreUtils
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8      as C8
import qualified ISX.CE.DB                  as D
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
newtype SiteHref = SiteHref { unSiteHref :: ByteString
    } deriving (Show)
instance RouteHref SiteHref D.SiteURL where
    toRouteHref s = SiteHref $ sites <> "/" <> fromRouteId s
    fromRouteHref h = do
        ["", "site", s] <- return $ C8.split '/' $ unSiteHref h
        toRouteId s
instance ToJSON SiteHref where
    toJSON o = toJSON (decodeUtf8 $ unSiteHref o :: Text)

newtype SitesHref = SitesHref { unSitesHref :: ByteString
    } deriving (Show)
instance RouteHref SitesHref () where
    toRouteHref _ = SitesHref sites
    fromRouteHref h = do
        ["", "site"] <- return $ C8.split '/' $ unSitesHref h
        pass
instance ToJSON SitesHref where
    toJSON o = toJSON (decodeUtf8 $ unSitesHref o :: Text)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
sites :: ByteString
sites = "/site"
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
instance RouteId D.SiteURL where
    toRouteId b = D.SiteURL <$>
        parseAbsoluteURI (decodeUtf8 $ B64.decodeLenient b)
    fromRouteId = B64.encodeUnpadded . show . D.unSiteURL
