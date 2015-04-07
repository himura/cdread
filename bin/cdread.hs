{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml as Yaml
import Network.FreeDB
import Network.FreeDB.Utils
import Network.HTTP.Conduit
import System.CDROM
import System.Console.GetOpt
import System.Environment
import System.IO
import Text.Printf

data Options = Options
    { device :: String
    , freeDBSetting :: FreeDBSetting
    } deriving Show

defaultOptions :: Options
defaultOptions =
    Options
    { device = "/dev/cdrom"
    , freeDBSetting = defaultFreeDBSetting
    }

defaultFreeDBSetting :: FreeDBSetting
defaultFreeDBSetting =
    FreeDBSetting
    { fdbUrl = "http://freedbtest.dyndns.org/~cddb/cddb.cgi"
    , fdbUserName = "user@example.jp"
    , fdbUserHostname = "example.jp"
    , fdbProxy = Nothing
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "d" ["device"]
          (ReqArg (\dev opts -> return $ opts { device = dev }) "device")
          ""
    ]

compilerOpts :: ([String] -> IO (t, [String])) -- ^ Error handler
             -> [OptDescr (t -> IO t)] -- ^ Option descriptions
             -> t -- ^ Default value of options
             -> [String] -- ^ argv
             -> IO (t, [String])
compilerOpts errHndl opts defaultOpts argv =
    case getOpt RequireOrder opts argv of
        (o,n,[]  ) -> (,) <$> foldl (>>=) (return defaultOpts) o <*> return n
        (_,_,errs) -> errHndl errs

usageMessage :: String -> [OptDescr (opt -> IO opt)] -> String
usageMessage programName = usageInfo
    (programName ++ " [OPTION...] <command> [<args>]\n\nThe " ++ programName ++ "\nThe global options are:")

run :: Options -> [String] -> IO ()
run Options{..} ("cddb":_) = do
    toc <- readToc device
    withManager $ \mgr -> do
        entries <- liftIO $ freeDBQuery freeDBSetting toc mgr
        case entries of
            [(categ, discid, _title)] -> do
                -- liftIO . T.putStrLn $ T.intercalate " " [categ, discid, title]
                cddbAssoc <- liftIO $ freeDBRead freeDBSetting categ discid mgr
                let cddb = makeCDDB toc cddbAssoc
                liftIO . S8.putStrLn $ Yaml.encode cddb
                -- forM_ cddb $ \(k, v) ->
                --     liftIO . T.putStrLn $ T.concat [k, " = ", v]
            _ -> do
                liftIO $ putStrLn "Multiple Choices:"
                forM_ entries $ \(categ, discid, title) -> do
                    liftIO . T.putStrLn $ T.intercalate " " [categ, discid, title]

run _ ("generate-tags":file:_) = do
    mcddb <- Yaml.decodeFile file
    case mcddb of
        Just CDDB{..} ->
            forM_ cddbTrack $ \CDDBTrack{..} -> do
                withFile (printf "%02d.tag" trackNumber) WriteMode $ \fp -> do
                    put fp "ALBUM" cddbAlbum
                    mput fp "ALBUM ARTIST" cddbAlbumArtist
                    mput fp "DATE" cddbDate
                    put fp "GENRE" cddbGenre
                    put fp "DISCID" cddbDiscID
                    put fp "TRACKTOTAL" (T.pack . show $ cddbTrackTotal)
                    put fp "TRACKNUMBER" (T.pack . show $ trackNumber)
                    put fp "TITLE" trackTitle
                    mput fp "ARTIST" trackArtist
        Nothing ->
            return () -- error
  where
    put fp key val = T.hPutStrLn fp $ T.concat [key , "=", val]
    mput _fp _key Nothing = return ()
    mput fp key (Just val) = put fp key val
run _ _ = return () -- error

data CDDB = CDDB
    { cddbAlbum :: T.Text
    , cddbAlbumArtist :: Maybe T.Text
    , cddbDate :: Maybe T.Text
    , cddbGenre :: T.Text
    , cddbDiscID :: T.Text
    , cddbTrackTotal :: Int
    , cddbTrack :: [CDDBTrack]
    } deriving (Eq, Show)
instance FromJSON CDDB where
    parseJSON (Object o) =
        CDDB
        <$> o .:  "ALBUM"
        <*> o .:? "ALBUM ARTIST"
        <*> o .:? "DATE"
        <*> o .:  "GENRE"
        <*> o .:  "DISCID"
        <*> o .:  "TRACKTOTAL"
        <*> o .:  "track"
    parseJSON o = fail $ "CDDB unexpected " ++ show o
instance ToJSON CDDB where
    toJSON CDDB{..} =
        object [ "ALBUM" .= cddbAlbum
               , "ALBUM ARTIST" .= cddbAlbumArtist
               , "DATE" .= cddbDate
               , "GENRE" .= cddbGenre
               , "DISCID" .= cddbDiscID
               , "TRACKTOTAL" .= cddbTrackTotal
               , "track" .= cddbTrack
               ]

data CDDBTrack = CDDBTrack
    { trackNumber :: Int
    , trackTitle :: T.Text
    , trackArtist :: Maybe T.Text
    } deriving (Eq, Show)
instance FromJSON CDDBTrack where
    parseJSON (Object o) =
        CDDBTrack
        <$> o .:  "TRACKNUMBER"
        <*> o .:  "TITLE"
        <*> o .:  "ARTIST"
    parseJSON o = fail $ "CDDBTrack unexpected " ++ show o
instance ToJSON CDDBTrack where
    toJSON CDDBTrack{..} =
        object [ "TRACKNUMBER" .= trackNumber
               , "TITLE" .= trackTitle
               , "ARTIST" .= trackArtist
               ]

makeCDDB :: Toc -> [(T.Text, T.Text)] -> CDDB
makeCDDB toc entries =
    CDDB { cddbAlbum = album
         , cddbAlbumArtist = albumArtist
         , cddbDate = M.lookup "DYEAR" m
         , cddbGenre = fromMaybe "Unknown" $ M.lookup "DGENRE" m
         , cddbDiscID = T.pack $ printf "%x" (tocDiscId toc)
         , cddbTrackTotal = trackNum
         , cddbTrack = map makeCDDBTrack [0..(trackNum-1)]
         }
  where
    m = M.fromList entries
    (album, albumArtist) = case M.lookup "DTITLE" m of
        Nothing -> ("", Nothing)
        Just dtitle -> parseTitleAuthor dtitle
    trackNum = length . tocAddresses $ toc

    makeCDDBTrack index =
        CDDBTrack { trackNumber = index + 1
                  , trackTitle = title
                  , trackArtist = artist
                  }
      where
        (title, artist) = case M.lookup (T.pack $ "TTITLE" ++ show index) m of
            Nothing -> ("", Nothing)
            Just ttitle -> parseTitleAuthor ttitle

main :: IO ()
main = do
    (opts, cmds) <- getArgs >>= compilerOpts usage options defaultOptions
    run opts cmds
  where
    usage errs = do
        pn <- getProgName
        ioError $ userError $ concat errs ++ usageMessage pn options
