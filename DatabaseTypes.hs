{-# LANGUAGE OverloadedStrings #-}

module DatabaseTypes
    where
    import qualified Data.Map as M
    import qualified Data.Text as T

    type Artist = T.Text
    type AlbumTitle = T.Text
    type Year = Int
    type Song = T.Text
    type Songs = [Song]

    data Album = Album
        { artist :: Artist
        , albumTitle :: AlbumTitle
        , year :: Year
        , songs :: Songs
        } deriving (Show, Eq)

    type Albums = M.Map T.Text [Album]
