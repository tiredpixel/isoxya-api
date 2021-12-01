module Isoxya.API.Endpoint.Streamer (
    create,
    list,
    read,
    delete,
    ) where


import           Isoxya.API.Core
import           TiredPixel.Common.URI
import qualified Isoxya.DB             as D


create :: Handler b API ()
create = do
    d <- gets _db
    Just _ <- run notFound $ fStreamers AW d
    req_ <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just strmId <- D.cStreamer
        (unURIAbsolute $ streamerCURL req) (streamerCTag req) d
    Just strm <- D.rStreamer strmId d
    let r = genStreamer strm
    created (unStreamerHref $ streamerHref r) r

list :: Handler b API ()
list = do
    d <- gets _db
    Just _ <- run notFound $ fStreamers AR d
    cur <- parseReq
    strms <- D.lStreamer cur d
    setResLink (unStreamersHref (toRouteHref () :: StreamersHref))
        (formatTime . D.streamerInserted) strms
    rs <- forM strms $ \strm -> return $ genStreamer strm
    writeJSON rs

read :: Handler b API ()
read = do
    d <- gets _db
    Just (strm, _) <- run notFound $ fStreamer AR d
    writeJSON $ genStreamer strm

delete :: Handler b API ()
delete = do
    d <- gets _db
    Just (_, strmId) <- run notFound $ fStreamer AW d
    D.dStreamer strmId d
    noContent
