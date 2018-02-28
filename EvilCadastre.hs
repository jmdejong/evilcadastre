
{-# LANGUAGE OverloadedStrings #-}

module EvilCadastre where

import Control.Exception
import Data.List
import Data.Char
import System.IO
import qualified System.Directory as Dir
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified System.Random as Random

import Utils
import qualified GameField
import GameField (Field, empty, update)
import Player (Player(Player))
import qualified Command
import Command (Command)
import Serializer


parcelPath :: String -> FilePath
parcelPath user = "./home/" ++ user ++ "/.cadastre/evil.txt"


-- safely read a file: don't load too large files into memory
readUtf8File :: FilePath -> IO T.Text
readUtf8File path = do
    inputHandle <- openFile path ReadMode
    bytes <- BS.hGetNonBlocking inputHandle 8192
    return $ TE.decodeUtf8 bytes

appendFileIfReadable :: FilePath -> IO [(FilePath, T.Text)] -> IO [(FilePath, T.Text)]
appendFileIfReadable path iofiles =
    do
        files <- iofiles
        maybeFile <- try (readUtf8File path) :: IO (Either SomeException T.Text)
        case maybeFile of
                Left _ex -> return files
                Right text -> do
                    return ((path, text):files)


readFiles :: [FilePath] -> IO [(FilePath, T.Text)]
readFiles = foldr appendFileIfReadable (return [])

-- System.Directory.listDirectory is to new to use
-- but this is the same code
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (Dir.getDirectoryContents path)
  where f filename = filename /= "." && filename /= ".."

loadInstructions :: IO (Map.Map Player [Command])
loadInstructions = do
    userNames <- listDirectory "home"
    parcelTexts <- readFiles $ map parcelPath userNames
    let parcels = map (\(path, text) -> (getOwnerFromPath path, filterMaybe $ map Command.parse $ T.lines text)) parcelTexts
    return $ Map.fromList parcels

getOwnerFromPath :: FilePath -> Player
getOwnerFromPath path = case dropWhile (== ".") (splitDirectories path) of
    "home":owner:_xs -> Player  owner

makeJSONCadastre :: Field -> BS.ByteString
makeJSONCadastre = BSL.toStrict . Aeson.encode


decodeJSONCadastre :: BS.ByteString -> Field
decodeJSONCadastre jsonText = json
    where 
        Right json = decoded
        decoded = Aeson.eitherDecode (BSL.fromStrict jsonText) :: Either String Field

readPreviousCadastre :: FilePath -> IO Field
readPreviousCadastre path = do
    jsonText <- BS.readFile path
    let parcels = decodeJSONCadastre jsonText
    return parcels

writeSafe :: (FilePath -> a -> IO()) -> FilePath -> a -> IO()
writeSafe writer path dat = do
    let tempPath = path ++ ".tempfile"
    writer tempPath dat
    Dir.renameFile tempPath path

saveCadastre :: FilePath -> Field -> IO ()
saveCadastre path = writeSafe BS.writeFile path . makeJSONCadastre


