
{-# LANGUAGE DeriveGeneric #-}

module Area (
    Area(Area),
    contains,
    getLocations,
    expand
) where

import GHC.Generics

import Types

data Area = Area Location Int Int
    deriving (Show, Eq, Generic)

contains :: Area -> Location -> Bool
contains (Area minpos width height) point = x >= xmin && y >= ymin && x < xmax && y < ymax
    where
        (x, y) = point
        (xmin, ymin) = minpos
        xmax = xmin + width
        ymax = ymin + height

getLocations :: Area -> [Location]
getLocations (Area minpos width height) = [(x, y) | x <- [xmin..xmax-1], y <- [ymin..ymax-1]]
    where
        (xmin, ymin) = minpos
        xmax = xmin + width
        ymax = ymin + height

expand :: Area -> Int -> Area
expand (Area (x, y) width height) size = Area (x-size, y-size) (width+2*size) (height+2*size)
