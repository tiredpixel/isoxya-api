module Isoxya.API.Endpoint.Processor (
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
    Just _ <- run notFound $ fProcessors AW d
    req_ <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just procId <- D.cProcessor
        (unURIAbsolute $ processorCURL req) (processorCTag req) d
    Just proc <- D.rProcessor procId d
    let r = genProcessor proc
    created (unProcessorHref $ processorHref r) r

list :: Handler b API ()
list = do
    d <- gets _db
    Just _ <- run notFound $ fProcessors AR d
    cur <- parseReq
    procs <- D.lProcessor cur d
    setResLink (unProcessorsHref (toRouteHref () :: ProcessorsHref))
        (formatTime . D.processorInserted) procs
    rs <- forM procs $ \proc -> return $ genProcessor proc
    writeJSON rs

read :: Handler b API ()
read = do
    d <- gets _db
    Just (proc, _) <- run notFound $ fProcessor AR d
    writeJSON $ genProcessor proc

delete :: Handler b API ()
delete = do
    d <- gets _db
    Just (_, procId) <- run notFound $ fProcessor AW d
    D.dProcessor procId d
    noContent
