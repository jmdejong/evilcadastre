
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serializer where


import Data.Aeson
import qualified Data.Text as Text

import GameField (Field)
import Player(Player(Player))
import Area(Area)
import Buildings(Building(Farm, Granary, TownCentre, Tower))


instance ToJSON Player where
    toJSON (Player name) = String $ Text.pack name
--     toEncoding = genericToEncoding defaultOptions
instance ToJSONKey Player

instance FromJSON Player where
    parseJSON (String name) = return $ Player $ Text.unpack name
instance FromJSONKey Player




instance ToJSON Area where
    toEncoding = genericToEncoding defaultOptions 
instance FromJSON Area

instance ToJSON Field where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Field

instance ToJSON Building where
    toJSON Farm = String "Farm"
    toJSON TownCentre = String "TownCentre"
    toJSON Granary = String "Granary"
    toJSON Tower = String "Tower"


instance FromJSON Building where
    parseJSON (String "Farm") = return Farm
    parseJSON (String "TownCentre") = return TownCentre
    parseJSON (String "Granary") = return Granary
    parseJSON (String "Tower") = return Tower


