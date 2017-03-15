{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.State
import Selector
import Database
import DatabaseTypes

import Text.Read
import System.Exit
import System.Environment
import System.Directory

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Data.Map as M
import qualified Data.List as L

type Arg = String

main :: IO ()
main = do 
    args <- getArgs
    path <- getEnv "MUSICDB"
    run args path

run :: [String] -> DatabasePath -> IO ()
run args dbPath = do
    db <- getDatabase dbPath
    processArgs args dbPath db 

getDatabase :: DatabasePath -> IO Albums
getDatabase path = do
    exists <- doesFileExist path 
    if exists
       then openDatabase path
       else return M.empty

openDatabase :: DatabasePath -> IO Albums
openDatabase path = do
    albums <- openAlbums path
    case albums of
      Left _ -> die "Unable to load database"
      Right result -> return result

processArgs :: [Arg] -> DatabasePath -> Albums -> IO ()
processArgs ["-a"] path albums = addAlbum path albums
processArgs ["-d"] path albums = do
    mAlb <- selectAlbum albums
    case mAlb of
      Just alb -> deleteAlbum path albums alb
      Nothing -> die "No Album Selected" 
processArgs ["-l"] _ albums = do
    mAlb <- selectAlbum albums
    case mAlb of
      Just alb -> showAlbum alb
      Nothing -> die "No Album Selected"
processArgs _ _ _ = putStrLn "Usage -l list | -a add | -d delete | -h usage"

selectAlbum :: Albums -> IO (Maybe Album)
selectAlbum albums = do
    (y, _) <- runStateT navigate (artistLevel albums, (0, 0))
    case y of
      Just res -> return $ getResult albums res
      Nothing -> return Nothing

showAlbum :: Album -> IO ()
showAlbum alb = mapM_ TextIO.putStrLn $ songs alb

deleteAlbum :: String -> Albums -> Album -> IO ()
deleteAlbum path albums alb = do
    let deletion = removeAlbum albums alb
    case deletion of 
      Just x -> saveAlbums path x >> putStrLn "Deleted" >> exit
      Nothing -> putStrLn "Error" >> exit

removeAlbum :: Albums -> Album -> Maybe Albums
removeAlbum albums alb = do
    artistAlbums <- M.lookup (artist alb) albums 
    idx <- L.elemIndex alb artistAlbums 
    deletedArtistAlbs <- Just $ delete artistAlbums idx
    Just $ removeAlbumOrArtist albums (artist alb) deletedArtistAlbs

removeAlbumOrArtist :: Albums -> Artist -> [Album] -> Albums
removeAlbumOrArtist albums artst artistAlbs 
  | null artistAlbs = M.delete artst albums
  | otherwise = M.insert artst artistAlbs albums

addAlbum :: String -> Albums -> IO ()
addAlbum path albums = do
    putStrLn "Enter Artist Name: "
    nArtist <- getText

    putStrLn "Enter Album Name: "
    nAlbum <- getText

    if not (albumExists albums nAlbum nArtist)
        then do
            putStrLn "Enter Album Year: "
            nYear <- getLineInt

            putStrLn "Enter Album Songs: Press enter between songs. Press enter twice to stop"
            nSongs <- getNewSongs []

            let newAlbum = Album nArtist nAlbum nYear nSongs
            let nAlbums = M.insertWith' (++) nArtist [newAlbum] albums

            saveAlbums path nAlbums >> exit
        else die "Album Already Exists"

albumExists :: Albums -> AlbumTitle -> Artist -> Bool
albumExists albs title artst = isJust $ M.lookup artst albs >>= \artstAlbs -> L.find (\x -> albumTitle x == title) artstAlbs

getNewSongs :: [Song] -> IO [Song]
getNewSongs x = do
    line <- getText
    if line == ""
        then return x
        else getNewSongs (x++ [line])

getLineInt :: IO Int
getLineInt = do
    line <- getLine
    case readMaybe line of
      Just x -> return x
      Nothing -> putStrLn "Invalid number entered" >> getLineInt

getText :: IO T.Text
getText = do
    str <- getLine
    return $ T.pack str

delete :: [a] -> Int -> [a]
delete xs n = let (ys,zs) = splitAt n xs   
          in ys ++ tail zs

exit :: forall a. IO a
exit = exitSuccess

die :: forall b. String -> IO b
die err = do 
    putStrLn err
    exitWith (ExitFailure 1)
