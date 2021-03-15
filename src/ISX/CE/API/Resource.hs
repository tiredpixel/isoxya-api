{-# LANGUAGE RecordWildCards #-}


module ISX.CE.API.Resource (
    Apex(..),
    ) where


import Data.Aeson
import Data.Time.Clock
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
data Apex = Apex {
    apexTNow    :: UTCTime,
    apexVersion :: Text
    } deriving (Show)
instance ToJSON Apex where
    toJSON Apex{..} = object [
        "t_now"    .= apexTNow,
        "version"  .= apexVersion]
