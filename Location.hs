{-# LANGUAGE DeriveGeneric #-}

module Location (
    Location(Location),
    globalPos,
    fromGlobalPos
) where

import GHC.Generics

import Types

data Location = Location Pos
    deriving (Eq, Show, Ord, Generic)

globalPos :: Location -> (Int, Int)
globalPos (Location (x, y)) = (x, y)


fromGlobalPos :: (Int, Int) -> Location
fromGlobalPos (x, y) = Location (x, y)
