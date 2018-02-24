{-# LANGUAGE DeriveGeneric #-}

module Player (
    Player(Player),
    name
) where

import GHC.Generics

data Player = Player {
    name :: String
} deriving (Show, Eq, Ord, Generic)
