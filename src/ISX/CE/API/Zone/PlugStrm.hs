module ISX.CE.API.Zone.PlugStrm (
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
    Just _ <- run notFound $ fPlugStrms AW d
    req_     <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just pId <- D.cPlugStrm
        (unURIAbsolute $ plugStrmCURL req) (plugStrmCTag req) d
    Just p <- D.rPlugStrm pId d
    let r = plugStrm p
    created (unPlugStrmHref $ plugStrmHref r) r

list :: Handler b API ()
list = do
    d <- gets _db
    Just _ <- run notFound $ fPlugStrms AR d
    cur <- parseReq
    ps <- D.lPlugStrm cur d
    setResLink (unPlugStrmsHref (toRouteHref () :: PlugStrmsHref))
        (formatTime . D.plugStrmTIns) ps
    rs <- forM ps $ \p -> return $ plugStrm p
    writeJSON rs

read :: Handler b API ()
read = do
    d <- gets _db
    Just (p, _) <- run notFound $ fPlugStrm AR d
    writeJSON $ plugStrm p

delete :: Handler b API ()
delete = do
    d <- gets _db
    Just (_, pId) <- run notFound $ fPlugStrm AW d
    D.dPlugStrm pId d
    noContent
