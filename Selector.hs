{-# LANGUAGE DeriveDataTypeable #-}

module Selector(navigate, artistLevel, getResult)
  where

    import Control.Monad.State
    import qualified Text.Read as TextRead
    import qualified Data.Text as T
    import qualified Data.Text.IO as TextIO
    import qualified Data.Map as M
    import qualified Data.List as L

    import Database
    import DatabaseTypes

    type InputSelection = String
    type Selection = Int
    type Result = (Selection, Selection)

    data ResultType = Continue | Done | Fail deriving (Show, Eq)
    data LevelControls = Next | Previous deriving (Show, Eq)
    type LevelState = (Level, Result)

    type NavigationState = StateT LevelState IO 

    data Level = Level 
        { nextLevel :: Maybe Level
        , prevLevel :: Maybe Level
        , quitLetter :: T.Text
        , printMessage :: T.Text
        , list :: Result -> Maybe [T.Text]
        , validSelection :: Result -> Int -> Bool
        , addResult :: Int -> Result -> Result
        }

    artistLevel :: Albums -> Level
    artistLevel albs = Level 
        { nextLevel = Just $ albumLevel albs
        , prevLevel = Nothing
        , quitLetter = T.pack "q"
        , printMessage = T.pack "Please enter number or \"q\" to quit"
        , list = \_ -> Just $ getArtistTxt albs
        , validSelection = \_ x -> x >= 0 && x < length (getArtistTxt albs)
        , addResult = \x (_, r2) -> (x, r2)
        }

    albumLevel :: Albums -> Level
    albumLevel albs = Level 
        { nextLevel = Nothing
        , prevLevel = Just $ artistLevel albs
        , quitLetter = T.pack "a"
        , printMessage = T.pack "Please enter number or \"a\" to go up a level"
        , list = \(x, _) -> getAlbumTxt albs (getArtistTxt albs !! x)
        , validSelection = \(x, _) y-> y >= 0 && y < maybe 0 length (getAlbumTxt albs (getArtistTxt albs !! x))
        , addResult = \x (r1, _) -> (r1, x)
        }

    navigate :: NavigationState (Maybe Result)
    navigate = do
        (lvl, results) <- get
        liftIO $ putStrLn ""
        liftIO $ mapM_ enumeratedPrinter $ enumerate $ maybe [] (map T.unpack ) $ list lvl results
        liftIO $ TextIO.putStrLn (printMessage lvl)
        selection <- liftIO getLine
        liftIO $ putStrLn ""

        continue <- parseInput selection
        (_, updatedResults) <- get
        case continue of
          Continue -> navigate
          Done -> return (Just updatedResults)
          Fail -> return Nothing

    parseInput :: InputSelection -> NavigationState ResultType
    parseInput selection = do
        (lvl, results) <- get
        parseInput' lvl results selection

    parseInput' :: Level -> Result -> String -> NavigationState ResultType
    parseInput' lvl results str 
      | quitLetter lvl == T.pack str = navigateNext Previous
      | otherwise = maybe (return Continue) validate (TextRead.readMaybe str)
          where validate x = if validSelection lvl results x
                     then put (lvl, addResult lvl x results) >> navigateNext Next
                     else return Continue

    navigateNext :: LevelControls -> NavigationState ResultType
    navigateNext nxt = do
        (lvl, results) <- get
        case (if nxt == Next then nextLevel else prevLevel) lvl of
            Nothing -> return (if nxt == Next then Done else Fail)
            Just x -> put (x, results) >> return Continue

    getArtistTxt  :: Albums -> [Artist]
    getArtistTxt albums = L.sort $ map fst $ M.toList albums

    getAlbumTxt :: Albums -> Artist -> Maybe [AlbumTitle]
    getAlbumTxt albs artst = M.lookup artst albs >>= \x -> Just $ L.sort $ map albumTitle x

    getResult :: Albums -> Result -> Maybe Album
    getResult albs (art, alb) = do -- So innefficient
        let artistTxt = getArtistTxt albs !! art
        albumTxt <- getAlbumTxt albs artistTxt >>= \x -> Just (x !! alb)
        artistAlbs <- M.lookup artistTxt albs 
        L.find (\x -> albumTitle x == albumTxt) artistAlbs
        
    enumerate :: [b] -> [(Integer, b)]
    enumerate = zip [0..]

    enumeratedPrinter :: (Integer, String) -> IO ()
    enumeratedPrinter = putStrLn . (\x -> show (fst x) ++ ": " ++ snd x)
