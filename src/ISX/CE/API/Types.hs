{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module ISX.CE.API.Types (
    API(..),
    ) where


import           Control.Lens (makeLenses)
import qualified ISX.CE.DB    as D


newtype API = API {
    _db :: D.Conn}

makeLenses ''API
