{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Isoxya.API.Type (
    API(..),
    ) where


import           Control.Lens (makeLenses)
import qualified Isoxya.DB    as D
import qualified Isoxya.Msg   as M


data API = API
             { _msgCrl :: M.ChanCrawler
             , _db     :: D.Conn
             }

makeLenses ''API
