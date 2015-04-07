{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.FreeDB
import Network.HTTP.Conduit
import System.CDROM
import System.Console.GetOpt
import System.Environment

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

run :: Options -> IO ()
run Options{..} = do
    toc <- readToc device
    withManager $ \mgr -> do
        entries <- liftIO $ freeDBQuery freeDBSetting toc mgr
        case entries of
            [(categ, discid, title)] -> do
                liftIO . T.putStrLn $ T.intercalate " " [categ, discid, title]
                cddb <- liftIO $ freeDBRead freeDBSetting categ discid mgr
                forM_ cddb $ \(k, v) ->
                    liftIO . T.putStrLn $ T.concat [k, " = ", v]
            _ -> do
                liftIO $ putStrLn "Multiple Choices:"
                forM_ entries $ \(categ, discid, title) -> do
                    liftIO . T.putStrLn $ T.intercalate " " [categ, discid, title]


main :: IO ()
main = do
    (gopts, _cmds) <- getArgs >>= compilerOpts usage options defaultOptions
    run gopts
  where
    usage errs = do
        pn <- getProgName
        ioError $ userError $ concat errs ++ usageMessage pn options
