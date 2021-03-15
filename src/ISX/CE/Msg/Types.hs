module ISX.CE.Msg.Types (
    ChanCrwl,
    CrwlPageId(..),
    MsgCrwl,
    ) where


import           Control.Concurrent.Chan
import qualified ISX.CE.DB               as D


type ChanCrwl = Chan MsgCrwl

data CrwlPageId = CrwlPageId {
    crwlPageIdSiteId :: D.SiteId,
    crwlPageIdSiteV  :: D.SiteV,
    crwlPageIdPageId :: D.PageId
    } deriving (Show)

type MsgCrwl = (D.SiteId, CrwlPageId)
