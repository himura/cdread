{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.FreeDB
       where

import Control.Applicative
import Data.Attoparsec.Text as Atto
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char
import Data.List
import qualified Data.Text as T
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

freeDBQueryResponseParser :: Parser (Int, [(T.Text, T.Text, T.Text)])
freeDBQueryResponseParser = do
    code <- decimal <* skipSpace1
    case code of
        200 -> do
            entry <- parseEachLine
            return (code, [entry])
        210 -> do
            -- consume message text: Found exact matches, list follows ...
            _msg <- Atto.skipWhile (not . isEndOfLine) <* skipSpace
            entries <- reverse <$> parseMultipleChoices []
            return (code, entries)
        _ -> return (code, [])

  where
    parseEachLine = do
        genre <- Atto.takeTill isHorizontalSpace <* skipSpace1
        discid <- Atto.takeWhile1 (inClass "0-9A-Fa-f") <* skipSpace1
        title <- Atto.takeWhile1 (not . isEndOfLine) <* skipSpace1
        return (genre, discid, title)
    parseMultipleChoices store = do
        (string "." >> return store) <|> (parseEachLine >>= \p -> parseMultipleChoices (p:store))

skipSpace1 :: Parser ()
skipSpace1 = Atto.takeWhile1 isSpace >> return ()

makeFreeDBReadRequest :: FreeDBSetting
                      -> String -- ^ caregory
                      -> String -- ^ discid
                      -> IO Request
makeFreeDBReadRequest setting categ discid =
    makeFreeDBRequest setting $ S8.pack $ "cddb read " ++ categ ++ " " ++ discid

freeDBReadResponseParser :: Parser (Int, [(T.Text, T.Text)])
freeDBReadResponseParser = do
    code <- decimal <* skipSpace1
    case code of
        210 -> do
            -- consume message text
            _msg <- Atto.skipWhile (not . isEndOfLine) <* skipSpace
            entries <- reverse <$> parseCddbLines
            return (code, entries)
        _ -> return (code, [])
  where
    skipComment = string "#" *> Atto.skipWhile (not . isEndOfLine) <* skipSpace
    parseCddbLines = do
        manyTill (many skipComment *> parseEachCddbLine <* many skipComment) (string ".")
    parseEachCddbLine = do
        key <- Atto.takeWhile (/= '=')
        _ <- string "="
        value <- Atto.takeWhile (not . isEndOfLine) <* skipSpace1
        return (key, value)
