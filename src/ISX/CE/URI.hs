module ISX.CE.URI (
    URIPage(..),
    URISite(..),
) where


import           Data.Aeson
import           Network.URI
import           TPX.Com.URI
import qualified Network.HTTP.Conduit as HTTP


newtype URIPage = URIPage { unURIPage :: URI
    } deriving (Show, Eq, Ord)
instance FromJSON URIPage where
    parseJSON = withText "URIPage" $
        maybe (fail "invalid URIPage") (pure . URIPage) .
        parsePageURI . parseRelativeReference . toString
instance ToJSON URIPage where
    toJSON = toJSON . unURIPage

newtype URISite = URISite { unURISite :: URI
    } deriving (Show, Eq, Ord)
instance FromJSON URISite where
    parseJSON = withText "URISite" $
        maybe (fail "invalid URISite") (pure . URISite) .
        parseSiteURI . parseAbsoluteURI . toString
instance ToJSON URISite where
    toJSON = toJSON . unURISite


parseSiteURI :: Maybe URI -> Maybe URI
parseSiteURI url = do
    url' <- url
    reqURISite <$> (HTTP.parseRequest . show) url'

parsePageURI :: Maybe URI -> Maybe URI
parsePageURI url = do
    url' <- url
    reqURIPage <$> (HTTP.parseRequest . show) url'
