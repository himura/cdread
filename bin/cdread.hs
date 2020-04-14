{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as S8
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml as Yaml
import Network.FreeDB
import Network.FreeDB.Utils
import Network.HTTP.Conduit
import qualified Network.URI as URI
import qualified Options.Applicative as Opts
import System.CDROM
import System.Environment
import System.IO
import Text.Printf

data CDDBOptions = CDDBOptions
    { device :: String
    , freeDBUrl :: String
    , freeDBUser :: String
    , freeDBUserHostname :: String
    , proxyUrl :: Maybe String
    } deriving Show

defaultCDDBOptions :: IO CDDBOptions
defaultCDDBOptions = do
    proxy <- getProxyEnv
    return $
        CDDBOptions
        { device = "/dev/cdrom"
        , freeDBUrl = "http://freedbtest.dyndns.org/~cddb/cddb.cgi"
        , freeDBUser = "user@example.jp"
        , freeDBUserHostname = "example.jp"
        , proxyUrl = proxy
        }

getProxyEnv :: IO (Maybe String)
getProxyEnv = do
    env <- M.fromList <$> getEnvironment
    return $
        M.lookup "https_proxy" env <|>
        M.lookup "HTTPS_PROXY" env <|>
        M.lookup "http_proxy" env

parseProxy :: String -> Maybe Proxy
parseProxy uri = Proxy <$> (S8.pack . URI.uriRegName <$> u) <*> (parsePort . URI.uriPort <$> u)
  where
    u = URI.parseURI uri >>= URI.uriAuthority
    parsePort :: String -> Int
    parsePort []       = 8080
    parsePort (':':xs) = read xs
    parsePort xs       = error $ "port number parse failed " ++ xs

cddbOptions :: CDDBOptions -- ^ default options
        -> Opts.Parser CDDBOptions
cddbOptions CDDBOptions{..} = CDDBOptions
    <$> Opts.strOption
        (  Opts.long "device"
        <> Opts.short 'd'
        <> Opts.metavar "DEVICE"
        <> Opts.help "CD-ROM device"
        <> Opts.value device )
    <*> Opts.strOption
        (  Opts.long "freedb"
        <> Opts.metavar "URL"
        <> Opts.help "FreeDB URL"
        <> Opts.value freeDBUrl )
    <*> Opts.strOption
        (  Opts.long "freedb-user"
        <> Opts.metavar "EMAIL"
        <> Opts.help "FreeDB User name"
        <> Opts.value freeDBUser )
    <*> Opts.strOption
        (  Opts.long "freedb-user-hostname"
        <> Opts.metavar "HOSTNAME"
        <> Opts.help "FreeDB User hostname"
        <> Opts.value freeDBUserHostname )
    <*> strOptional proxyUrl
        (  Opts.long "proxy"
        <> Opts.metavar "URI"
        <> Opts.help "proxy uri (example http://proxy.example.com:8080/)" )
  where
    strOptional def flags = Just <$> Opts.strOption flags <|> pure def

subcommand :: CDDBOptions -> Opts.Parser (IO ())
subcommand defCddbOpt =
    Opts.subparser
    (  Opts.command "cddb" (Opts.info (runCddb <$> (Opts.helper <*> cddbOptions defCddbOpt)) Opts.idm)
    <> Opts.command "gentags" (Opts.info (runGenTags <$> (Opts.helper <*> Opts.strArgument (Opts.metavar "CDDB.yml"))) Opts.idm)
    )

runCddb :: CDDBOptions -> IO ()
runCddb opt = do
    toc <- readToc (device opt)
    let query = obtainFreeDBQueryString toc
    putStrLn $ "# FreeDB Query: " ++ query
    mgr <- newManager tlsManagerSettings
    entries <- liftIO $ freeDBQuery freeDBSetting toc mgr
    case entries of
        [(categ, discid, _title)] -> putEachEntries toc categ discid mgr
        _ -> do
            liftIO $ putStrLn "Multiple Choices:"
            forM_ entries $ \(categ, discid, title) -> do
                liftIO . T.putStrLn $ T.intercalate " " [categ, discid, title]
                putEachEntries toc categ discid mgr

  where
    freeDBSetting = FreeDBSetting
        { fdbUrl = freeDBUrl opt
        , fdbUserName = freeDBUser opt
        , fdbUserHostname = freeDBUserHostname opt
        , fdbProxy = proxyUrl opt >>= parseProxy
        }

    putEachEntries toc categ discid mgr = do
        -- liftIO . T.putStrLn $ T.intercalate " " [categ, discid, title]
        cddbAssoc <- liftIO $ freeDBRead freeDBSetting categ discid mgr
        let cddb = makeCDDB toc cddbAssoc
        liftIO . S8.putStrLn $ Yaml.encode cddb
        -- forM_ cddb $ \(k, v) ->
        --     liftIO . T.putStrLn $ T.concat [k, " = ", v]

runGenTags :: FilePath -> IO ()
runGenTags file = do
    mcddb <- Yaml.decodeFile file
    case mcddb of
        Just o -> genTags o
        Nothing -> return () -- error

genTags :: Value -> IO ()
genTags o =
    forM_ (o ^.. key "track" . _Array . traversed) $ \track -> do
        writeTagFile (track ^? key "TRACKNUMBER" . _Integral) track
  where
    commonField = o ^. _Object . to (HashMap.delete "track")

    writeTagFile :: Maybe Int -> Value -> IO ()
    writeTagFile Nothing track =
        putStrLn $ "Unknown track number: " ++ show track
    writeTagFile (Just tracknum) track = do
        let fields = HashMap.union (track ^. _Object) commonField
        withFile (printf "%02d.tag" tracknum) WriteMode $ \fp ->
            forM_ (HashMap.toList fields) $ \(k, vobj) -> do
                case showValue vobj of
                    Just v -> T.hPutStrLn fp $ T.concat [k, "=", v]
                    Nothing -> T.hPutStrLn fp $ T.concat ["# unknown value: ", k, "=", T.pack . show $ vobj]
    showValue :: Value -> Maybe T.Text
    showValue (String s) = Just s
    showValue v@(Number _i) = T.pack . show <$> (v ^? _Integral :: Maybe Int)
    showValue _ = Nothing

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
        nonNullObject
            [ "ALBUM" .= cddbAlbum
            , "ALBUM ARTIST" .= cddbAlbumArtist
            , "ARTIST" .= cddbAlbumArtist
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
        nonNullObject
            [ "TRACKNUMBER" .= trackNumber
            , "TITLE" .= trackTitle
            , "ARTIST" .= trackArtist
            ]

nonNullObject :: [Pair] -> Value
nonNullObject = object . filter ((/= Null) . snd)

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
    m = M.fromListWith (flip T.append) entries
    (album, albumArtist) = case M.lookup "DTITLE" m of
        Nothing -> ("", Nothing)
        Just dtitle -> parseTitleAuthor dtitle
    trackNum = length . tocAddresses $ toc

    makeCDDBTrack trackIndex =
        CDDBTrack { trackNumber = trackIndex + 1
                  , trackTitle = title
                  , trackArtist = artist
                  }
      where
        (title, artist) = case M.lookup (T.pack $ "TTITLE" ++ show trackIndex) m of
            Nothing -> ("", Nothing)
            Just ttitle -> parseTitleAuthor ttitle

main :: IO ()
main = do
    cddbOpt <- defaultCDDBOptions
    join $ Opts.execParser (Opts.info (Opts.helper <*> subcommand cddbOpt) Opts.idm)
