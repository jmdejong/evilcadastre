
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Command (
    Command(Build),
    parse
) where

import Location (Location(Location))
import qualified Buildings
import Buildings (Building(TownCentre, Farm, Granary, Tower))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Text.Read
import Utils
import Types

data Command = Build Location Building
    deriving (Eq, Show)

parse :: Text -> Maybe Command
parse line = do
    -- anyone has a good alternative for hungarian notation here?
    (sx, sy, sbuilding) <- 
        case Text.words line of
            [sx, sy, sbuilding] -> Just (sx, sy, sbuilding)
            _ -> Nothing
    x <- Text.Read.readMaybe $ Text.unpack sx
    y <- Text.Read.readMaybe $ Text.unpack sy
    building <- parseBuilding sbuilding
    Just (Build (Location (x, y)) building)


parseBuilding :: Text -> Maybe Building
parseBuilding text = case text of
    "towncentre" -> Just TownCentre
    "towncenter" -> Just TownCentre
    "farm" -> Just Farm
    "granary" -> Just Granary
    "tower" -> Just Tower
--     "cannonnorth" -> Just (Cannon North)
--     "cannonsouth" -> Just (Cannon South)
--     "cannoneast" -> Just (Cannon East)
--     "cannonwest" -> Just (Cannon West)
    "O" -> Just TownCentre
    "=" -> Just Farm
    "g" -> Just Granary
    "T" -> Just Tower
--     "^" -> Just (Cannon North)
--     "v" -> Just (Cannon South)
--     "<" -> Just (Cannon West)
--     ">" -> Just (Cannon East)
    _ -> Nothing

-- parseCommands :: [Text] -> [Command]
-- parseCommands = filterMaybe . map parse

        

-- globalPos :: Location -> (Int, Int)
-- globalPos (Location x y) = (x, y)

