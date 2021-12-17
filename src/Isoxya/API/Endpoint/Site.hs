module Isoxya.API.Endpoint.Site (
    create,
    read,
    ) where


import           Isoxya.API.Core
import           TiredPixel.Common.URI
import qualified Isoxya.DB             as D


create :: Handler b API ()
create = do
    d <- gets _db
    Just _ <- run notFound $ fSites d
    req_ <- getJSON' >>= validateJSON
    Just req <- runValidate req_
    Just stId <- D.cSite (unURISite $ siteCURL req) d
    Just st <- D.rSite stId d
    let r = genSite st
    created (unSiteHref $ siteHref r) r

read :: Handler b API ()
read = do
    d <- gets _db
    Just (st, _) <- run notFound $ fSite d
    writeJSON $ genSite st
