module ISX.CE.API.Auth (
    APerm(..),
    ) where


data APerm =
    AR | -- read
    AW   -- write
