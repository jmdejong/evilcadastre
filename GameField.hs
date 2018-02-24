{-# LANGUAGE DeriveGeneric #-}

module GameField (
    Field,
    empty,
    update
) where

import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Generics

import qualified Location
import Location (Location (Location))
import qualified Buildings
import Buildings (Building(TownCentre))
import qualified Player
import Player (Player)
import qualified Area
import Area (Area(Area))
import qualified Command
import Command (Command(Build))
import Utils
import Types

data Field = Field {
    buildingMap :: Map Location Building,
    playerAreas :: Map Player [Area]
} deriving (Eq, Show, Generic)


parcelWidth = 7
parcelHeight = 5

empty :: Field
empty = Field Map.empty Map.empty

areaBuildings :: Field -> Area -> [Building]
areaBuildings (Field buildings _) = 
    filterMaybe . map (buildings Map.!?) . Area.getLocations

playerBuildings :: Field -> Player -> [Building]
playerBuildings field player = case (playerAreas field) Map.!? player of
    Just areas -> concatMap (areaBuildings field) areas
    Nothing -> []

playerProduction :: Field -> Player -> Resources
playerProduction field player = case playerBuildings field player of
            [] -> 10
            buildings -> max 2 $ sum $ map Buildings.production buildings

ownsLocation :: Field -> Player -> Location -> Bool
ownsLocation (Field _ players) player location = case players Map.!? player of
        Just areas -> elem (locationParcel location) areas
        Nothing -> False
--     any (\area -> Area.contains area location) $ (playerAreas field) Map.! player

-- canBuild :: Field -> Player -> Building -> Location -> Bool
-- canBuild field player TownCentre location = notElem TownCentre $ areaBuildings field (locationParcel location)
-- canBuild field player _building location = ownsLocation field player location

locationParcel :: Location -> Area
locationParcel (Location (x, y)) = Area (Location (x `div` parcelWidth, y `div` parcelHeight)) parcelWidth parcelHeight

build :: Field -> Location -> Building -> Player -> Maybe Field
build field@(Field buildings players) location building player = case building of 
    TownCentre -> if canClaim location
        then Just claimedField
        else Nothing
    _ -> if ownsLocation field player location 
        then Just $ Field newMap players
        else Nothing
    where
        claimedField = Field newMap $ Map.alter addClaim player players
        newMap = Map.alter (\_ -> Just building) location buildings
        canClaim location = notElem TownCentre $ areaBuildings field (locationParcel location)
        addClaim Nothing = Just [parcel]
        addClaim (Just areas) = Just (parcel:areas)
        parcel = locationParcel location

commandCost :: Field -> Command -> Resources
commandCost (Field buildings players) (Build location building) =
    case buildings Map.!? location of
        Just building -> Buildings.runCost building
        _ -> Buildings.buildCost building


-- perform a command if possible, else just ignore it
performCommand :: Field -> Player -> Resources -> Command -> Field-> (Field, Resources)
performCommand oldField player resources command@(Build location building) field =
    case (build field location building player, resources >= cost) of
         (Just newField, True) -> (newField, resources - cost)
         _ -> (field, resources)
--     if canBuild field player building location && cost <= resources
--        then (build field location building, resources - cost)
--        else (field, resources)
    where cost = commandCost oldField command

performCommands :: Field -> Player -> [Command] -> Field -> Field
performCommands oldField player commands field =
    fst $ foldl doCommand (field, max 2 $ playerProduction oldField player) commands
    where
        doCommand (field, resources) command = performCommand oldField player resources command field


update :: Field -> Map Player [Command] -> Field
update field commands = Map.foldrWithKey (performCommands field) empty commands
