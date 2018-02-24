
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Buildings (
    Building(TownCentre, Farm, Granary),
    Resources,
    production,
    buildCost,
    runCost
) where

import GHC.Generics

import qualified Data.Text as Text
import Data.Text (Text)

import Types


data Building = TownCentre | Farm | Granary -- | Cannon Direction
    deriving (Eq, Show)


production :: Building -> Resources
production Farm = 1
production Granary = 4
production _ = 0

buildCost :: Building -> Resources
buildCost TownCentre = 8
buildCost Farm = 2
buildCost Granary = 4
-- buildCost (Cannon _) = 10

runCost :: Building -> Resources
runCost TownCentre = 1
runCost Granary = 4
-- runCost (Cannon _) = 2
runCost _ = 0

