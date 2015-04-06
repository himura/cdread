{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.FreeDB
       where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.List
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT
import System.CDROM
import Text.Printf

data FreeDBSetting = FreeDBSetting
    { fdbUrl :: String
    , fdbUserName :: String
    , fdbUserHostname :: String
    , fdbProxy :: Maybe Proxy
    } deriving (Show, Eq)

obtainFreeDBQueryString :: Toc -> String
obtainFreeDBQueryString toc =
    concat . intersperse " " $ (discid : len : addresses) ++ [show (tocLengthOfDiscInSec toc)]
  where
    discid = printf "%x" $ tocDiscId toc
    len = show . length $ tocAddresses toc
    addresses = map show (tocAddresses toc)

makeFreeDBRequest :: FreeDBSetting
                  -> S.ByteString
                  -> IO Request
makeFreeDBRequest FreeDBSetting{..} cmd = do
    req <- parseUrl fdbUrl
    return $ req { proxy = fdbProxy
                 , queryString = HT.renderSimpleQuery False query
                 }
  where
    query = [ ("cmd", cmd)
            , ("hello", S8.pack $ fdbUserName ++ " " ++ fdbUserHostname ++ " Haskell v1")
            , ("proto", "6")
            ]

makeFreeDBQueryRequest :: FreeDBSetting
                       -> String
                       -> IO Request
makeFreeDBQueryRequest setting reqStr =
    makeFreeDBRequest setting $ S8.pack $ "cddb query " ++ reqStr

makeFreeDBReadRequest :: FreeDBSetting
                      -> String -- ^ caregory
                      -> String -- ^ discid
                      -> IO Request
makeFreeDBReadRequest setting categ discid =
    makeFreeDBRequest setting $ S8.pack $ "cddb read " ++ categ ++ " " ++ discid

-- fetchFreeDB :: FilePath
--             -> FreeDBSetting
--             -> Manager
--             -> IO Response
-- fetchFreeDB dev setting mgr = do
--     qstr <- obtainFreeDBQueryString dev
--     req <- makeFreeDBRequest setting qstr
--     response <- httpLbs req mgr
