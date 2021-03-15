module ISX.CE.API.Zone.Site (
    create,
    read,
    ) where


import           ISX.CE.API.Core
import           ISX.CE.URI
import qualified ISX.CE.DB       as D


create :: Handler b API ()
create = do
    d <- gets _db
    Just _   <- run notFound $ fSites AW d
    req_     <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just sId <- D.cSite (unURISite $ siteCURL req) d
    Just s <- D.rSite sId d
    let r = site s
    created (unSiteHref $ siteHref r) r

read :: Handler b API ()
read = do
    d <- gets _db
    Just (s, _) <- run notFound $ fSite AR d
    writeJSON $ site s
