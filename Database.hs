{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database (openAlbums, saveAlbums, getAlbumList, DatabasePath)
    where

    import System.Directory
    import Control.Applicative
    import qualified Data.Map as M
    import qualified Data.Text as T
    import qualified Data.List as L
    import Data.Attoparsec.Text

    import DatabaseTypes

    type DatabasePath = String

    -- Private API
    parseAlbum :: Parser Album
    parseAlbum = do
        artist <- takeTill (== '\n')
        char '\n'
        year <- decimal
        album <- char ' ' *> takeTill (== '\n')
        char '\n'
        songs <- manyTill (char '-' *> takeTill (== '\n') <* endOfLine) (endOfLine <|> endOfInput)
        return $ Album artist album year songs

    addAlbum :: Albums -> Album -> Albums
    addAlbum albs alb = M.insertWith' (++) (artist alb) [alb] albs

    addAlbums :: [Album] -> Albums -> Albums
    addAlbums albsList albsMap
      | null albsList = albsMap
      | otherwise = addAlbums x $ addAlbum albsMap y
        where x = tail albsList
              y = head albsList

    parseAlbums :: Parser Albums
    parseAlbums = do
        albums <- many' parseAlbum
        let albMap = M.empty
        return $ addAlbums albums albMap

    foldSongs :: Songs -> String
    foldSongs x = L.intercalate "\n" (map (\y -> "-" ++ T.unpack y) x)

    foldAlbum :: Album -> String
    foldAlbum (Album artist album year songs) = aArtist ++ "\n" ++ aYear ++ " " ++ aAlbum ++ "\n" ++ strSongs ++ "\n"
      where strSongs = foldSongs songs 
            aArtist = T.unpack artist
            aAlbum = T.unpack album
            aYear = show year

    writeAlbums :: Albums -> String
    writeAlbums albums = L.intercalate "\n" (map foldAlbum (getAlbumList albums))

    getAlbumList :: Albums -> [Album]
    getAlbumList albums = concatMap snd $ M.toList albums

    -- Public AP
    openAlbums :: DatabasePath -> IO (Either String Albums)
    openAlbums path = do
        input <- readFile path
        return $ parseOnly parseAlbums $ T.pack input

    saveAlbums :: DatabasePath -> Albums -> IO ()
    saveAlbums path albums = do
        writeFile (path ++ ".tmp") (writeAlbums albums)
        renameFile (path ++ ".tmp") path
