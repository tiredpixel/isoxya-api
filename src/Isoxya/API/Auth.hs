module Isoxya.API.Auth (
    APerm(..),
    ) where


data APerm =
    AR | -- read
    AW   -- write
