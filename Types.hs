module Types where

-- somewhat like the utils module but for types instead of functions

type Resources = Integer

type Pos = (Int, Int)

data Direction = North | South | East | West
    deriving (Eq, Show)
