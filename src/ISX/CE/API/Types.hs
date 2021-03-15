{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module ISX.CE.API.Types (
    API(..),
    ) where


import           Control.Lens (makeLenses)
import qualified ISX.CE.DB    as D
import qualified ISX.CE.Msg   as M


data API = API {
    _msgCrwl :: M.ChanCrwl,
    _db      :: D.Conn}

makeLenses ''API
