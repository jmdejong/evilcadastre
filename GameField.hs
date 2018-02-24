{-# LANGUAGE DeriveGeneric #-}

module GameField (
    Field,
    empty,
    update,
    playerProduction,
    commandCost
) where

import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Generics
import qualified Data.List as List

import qualified Location
import Location (Location (Location))
import qualified Buildings
import Buildings (Building(TownCentre, Tower))
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
towerRange = 2

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
            buildings -> max 3 $ sum $ map Buildings.production buildings

ownsArea :: Field -> Player -> Area -> Bool
ownsArea (Field _ players) player area = case players Map.!? player of
        Just areas -> elem area areas
        Nothing -> False

ownsLocation :: Field -> Player -> Location -> Bool
ownsLocation field player location = ownsArea field player $ locationParcel location
--     case players Map.!? player of
--         Just areas -> elem (locationParcel location) areas
--         Nothing -> False


locationParcel :: Location -> Area
locationParcel (Location (x, y)) = Area (Location ((x `div` parcelWidth) * parcelWidth, (y `div` parcelHeight) * parcelHeight)) parcelWidth parcelHeight

claim :: Field -> Field -> Area -> Player -> Maybe Field
claim oldField@(Field oldBuildings oldClaims) field@(Field buildings claims) area player = 
    if (ownsArea oldField player area) || (
            (not $ isClaimed area) && (
                (length playerTowers > length otherTowers) ||
                (playerBuildings oldField player == [])))
        then Just $ Field buildings $ Map.alter addClaim player claims
        else Nothing
    where
        isClaimed area = elem area $ concat $ Map.elems oldClaims
        playerTowers :: [Location]
        otherTowers :: [Location]
        (playerTowers, otherTowers) = List.partition (ownsLocation oldField player) nearTowers
        nearTowers = filter ((== Just Tower) . (oldBuildings Map.!?)) $Area.getLocations $ Area.expand area towerRange
        addClaim Nothing = Just [area]
        addClaim (Just areas) = Just (area:areas)
    

build :: Field -> Field -> Location -> Building -> Player -> Maybe Field
build oldField field@(Field buildings players) location building player = case building of 
    TownCentre -> case claim oldField field (locationParcel location) player of 
        Just (Field buildings newClaims) -> Just $ Field newMap newClaims
        Nothing -> Nothing
--     if canClaim location
--         then Just claimedField
--         else Nothing
    _ -> if ownsLocation field player location 
            then Just $ Field newMap players
            else Nothing
    where
--         claimedField = Field newMap $ Map.alter addClaim player players
        newMap = Map.alter (\_ -> Just building) location buildings
--         isClaimed location = elem TownCentre $ areaBuildings field (locationParcel location)
--         canClaim location = not isClaimed and not
        parcel = locationParcel location

commandCost :: Field -> Command -> Resources
commandCost (Field buildings players) (Build location building) =
    case buildings Map.!? location of
        Just building -> Buildings.runCost building
        _ -> Buildings.buildCost building


-- perform a command if possible, else just ignore it
performCommand :: Field -> Player -> Resources -> Command -> Field-> (Field, Resources)
performCommand oldField player resources command@(Build location building) field =
    case (build oldField field location building player, resources >= cost) of
         (Just newField, True) -> (newField, resources - cost)
         _ -> (field, resources)
    where cost = commandCost oldField command

performCommands :: Field -> Player -> [Command] -> Field -> Field
performCommands oldField player commands field =
    fst $ foldl doCommand (field, max 2 $ playerProduction oldField player) commands
    where
        doCommand (field, resources) command = performCommand oldField player resources command field


update :: Field -> Map Player [Command] -> Field
update field commands = Map.foldrWithKey (performCommands field) empty commands
