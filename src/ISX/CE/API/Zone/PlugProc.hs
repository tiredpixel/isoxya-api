module ISX.CE.API.Zone.PlugProc (
    create,
    list,
    read,
    delete,
    ) where


import           ISX.CE.API.Core
import           TPX.Com.URI
import qualified ISX.CE.DB       as D


create :: Handler b API ()
create = do
    d <- gets _db
    Just _ <- run notFound $ fPlugProcs AW d
    req_     <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just pId <- D.cPlugProc
        (unURIAbsolute $ plugProcCURL req) (plugProcCTag req) d
    Just p <- D.rPlugProc pId d
    let r = plugProc p
    created (unPlugProcHref $ plugProcHref r) r

list :: Handler b API ()
list = do
    d <- gets _db
    Just _ <- run notFound $ fPlugProcs AR d
    cur <- parseReq
    ps <- D.lPlugProc cur d
    setResLink (unPlugProcsHref (toRouteHref () :: PlugProcsHref))
        (formatTime . D.plugProcTIns) ps
    rs <- forM ps $ \p -> return $ plugProc p
    writeJSON rs

read :: Handler b API ()
read = do
    d <- gets _db
    Just (p, _) <- run notFound $ fPlugProc AR d
    writeJSON $ plugProc p

delete :: Handler b API ()
delete = do
    d <- gets _db
    Just (_, pId) <- run notFound $ fPlugProc AW d
    D.dPlugProc pId d
    noContent
