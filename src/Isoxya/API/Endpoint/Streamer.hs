module Isoxya.API.Endpoint.Streamer (
    create,
    list,
    read,
    delete,
    ) where


import           Isoxya.API.Core
import qualified Isoxya.DB             as D
import           TiredPixel.Common.URI


create :: Handler b API ()
create = do
    d <- gets _db
    req_ <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just strId <- D.cStreamer (unURIAbsolute $ streamerCURL req)
        (streamerCTag req) d
    Just str <- D.rStreamer strId d
    let r = genStreamer str
    created (unStreamerHref $ streamerHref r) r

list :: Handler b API ()
list = do
    d <- gets _db
    cur <- parseReq
    strs <- D.lStreamer cur d
    setResLink (unStreamersHref (toRouteHref () :: StreamersHref))
        (formatTime . D.streamerInserted) strs
    rs <- forM strs $ \str -> return $ genStreamer str
    writeJSON rs

read :: Handler b API ()
read = do
    d <- gets _db
    Just (str, _) <- run notFound $ fStreamer d
    writeJSON $ genStreamer str

delete :: Handler b API ()
delete = do
    d <- gets _db
    Just (_, strId) <- run notFound $ fStreamer d
    D.dStreamer strId d
    noContent
