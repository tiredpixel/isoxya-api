module Isoxya.API.Endpoint.Site (
    create,
    read,
    ) where


import           Isoxya.API.Core
import           Isoxya.URI
import qualified Isoxya.DB       as D


create :: Handler b API ()
create = do
    d <- gets _db
    Just _ <- run notFound $ fSites AW d
    req_ <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just siteId <- D.cSite (unURISite $ siteCURL req) d
    Just site <- D.rSite siteId d
    let r = genSite site
    created (unSiteHref $ siteHref r) r

read :: Handler b API ()
read = do
    d <- gets _db
    Just (site, _) <- run notFound $ fSite AR d
    writeJSON $ genSite site
