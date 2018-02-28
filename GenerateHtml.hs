
{-# LANGUAGE OverloadedStrings #-}


import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map ((!?))

import EvilCadastre (readPreviousCadastre, writeSafe)
import qualified GameField
import GameField(Field, buildingMap, playerAreas)
import Buildings (Building (TownCentre, Farm, Granary, Tower))



width = 256
height = 256

htmlHeader :: Text
htmlHeader = "<!DOCTYPE html>\n<html>\n<!-- See github.com/jmdejong/evilcadastre for instructions -->\n<head>\n    <meta charset='utf-8'>\n   \n<style>\na {text-decoration: none}\n    </style>\n</head>\n<body>\n    <pre>"
htmlFooter :: Text
htmlFooter = "</body>\n made by ~troido-->\n</html>\n"


textFor :: Building -> Text
textFor building = case building of
    TownCentre -> "O"
    Farm -> "="
    Granary -> "g"
    Tower -> "T"
    otherwise -> "s?"

-- charFor :: Building -> Char
-- charFor building = case building of
--     TownCentre -> 'O'
--     Farm -> '='
--     Granary -> 'g'
--     Tower -> 'T'
--     otherwise -> '?'


generateHTML :: Field -> Text
generateHTML field = Text.concat [htmlHeader, fieldText, htmlFooter]
    where
        fieldText = Text.unlines fieldLines
        fieldLines = map (\y -> Text.concat $ map (`textAtPos` y) [0..width-1]) [0..height-1]
        textAtPos x y = case (buildingMap field) !? (x, y) of
            Just building -> textFor building
            Nothing -> " "

main :: IO ()
main = do
    field <- readPreviousCadastre "town.json"
    writeSafe Text.writeFile "town.html" $ generateHTML field
    print "Done"
