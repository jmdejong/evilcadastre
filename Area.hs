
{-# LANGUAGE DeriveGeneric #-}

module Area (
    Area(Area),
    contains,
    getLocations
) where

import GHC.Generics

import Location

data Area = Area Location Int Int
    deriving (Show, Eq, Generic)

contains :: Area -> Location -> Bool
contains (Area minpos width height) point = x >= xmin && y >= ymin && x < xmax && y < ymax
    where
        (x, y) = Location.globalPos point
        (xmin, ymin) = Location.globalPos minpos
        xmax = xmin + width
        ymax = ymin + height

getLocations :: Area -> [Location]
getLocations (Area minpos width height) = [Location.fromGlobalPos (x, y) | x <- [xmin..xmax-1], y <- [ymin..ymax-1]]
    where
        (xmin, ymin) = Location.globalPos minpos
        xmax = xmin + width
        ymax = ymin + height
