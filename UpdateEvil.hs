


import EvilCadastre (readPreviousCadastre, loadInstructions, saveCadastre)

import qualified GameField


updateCadastre :: Maybe FilePath -> IO () 
updateCadastre arg = do
    oldField <- case arg of
        Just path -> readPreviousCadastre path
        Nothing -> return GameField.empty
    instructions <- loadInstructions
    let cadastre = GameField.update oldField instructions
    saveCadastre "town.json" cadastre
    
--     let troido = Player.Player "troido"
    
--     print oldField
--     print $ GameField.playerProduction oldField troido
--     print $ instructions Map.! troido
--     print $ map (GameField.commandCost oldField) (instructions Map.! troido)
    print cadastre
    print "done"

main :: IO ()
main = do
    updateCadastre $ Just "town.json"
