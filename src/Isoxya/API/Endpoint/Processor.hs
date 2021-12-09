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
    Just proId <- D.cProcessor
        (unURIAbsolute $ processorCURL req) (processorCTag req) d
    Just pro <- D.rProcessor proId d
    let r = genProcessor pro
    created (unProcessorHref $ processorHref r) r

list :: Handler b API ()
list = do
    d <- gets _db
    Just _ <- run notFound $ fProcessors AR d
    cur <- parseReq
    pros <- D.lProcessor cur d
    setResLink (unProcessorsHref (toRouteHref () :: ProcessorsHref))
        (formatTime . D.processorInserted) pros
    rs <- forM pros $ \pro -> return $ genProcessor pro
    writeJSON rs

read :: Handler b API ()
read = do
    d <- gets _db
    Just (pro, _) <- run notFound $ fProcessor AR d
    writeJSON $ genProcessor pro

delete :: Handler b API ()
delete = do
    d <- gets _db
    Just (_, proId) <- run notFound $ fProcessor AW d
    D.dProcessor proId d
    noContent
