{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Network.FreeDB
       where

import Control.Applicative
import Control.Monad.Catch
import Data.Attoparsec.Text as Atto
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
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

data FreeDBError
    = ResponseParseError String T.Text
    | ResponseStatusError Int T.Text
    deriving (Eq, Show, Typeable)
instance Exception FreeDBError

freeDBQuery :: FreeDBSetting -> Toc -> Manager -> IO [(T.Text, T.Text, T.Text)]
freeDBQuery setting toc mgr = do
    req <- makeFreeDBQueryRequest setting $ obtainFreeDBQueryString toc
    res <- httpLbs req mgr
    let msg = T.decodeUtf8 . L.toStrict . responseBody $ res
    case parseFreeDBQueryResponse msg of
        Left err -> throwM err
        Right result -> return result

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

freeDBQueryResponseParser :: Parser (Int, Either T.Text [(T.Text, T.Text, T.Text)])
freeDBQueryResponseParser = do
    code <- decimal <* skipSpace1
    case code of
        200 -> do
            entry <- parseEachLine
            return (code, Right [entry])
        210 -> do
            -- consume message text: Found exact matches, list follows ...
            _msg <- Atto.skipWhile (not . isEndOfLine) <* skipSpace
            entries <- manyTill parseEachLine (string ".")
            return (code, Right entries)
        _ -> do
            body <- Atto.takeTill (const True)
            return (code, Left body)

  where
    parseEachLine = do
        genre <- Atto.takeTill isHorizontalSpace <* skipSpace1
        discid <- Atto.takeWhile1 (inClass "0-9A-Fa-f") <* skipSpace1
        title <- Atto.takeWhile1 (not . isEndOfLine) <* skipSpace1
        return (genre, discid, title)

skipSpace1 :: Parser ()
skipSpace1 = Atto.takeWhile1 isSpace >> return ()

makeFreeDBReadRequest :: FreeDBSetting
                      -> String -- ^ caregory
                      -> String -- ^ discid
                      -> IO Request
makeFreeDBReadRequest setting categ discid =
    makeFreeDBRequest setting $ S8.pack $ "cddb read " ++ categ ++ " " ++ discid

freeDBReadResponseParser :: Parser (Int, Either T.Text [(T.Text, T.Text)])
freeDBReadResponseParser = do
    code <- decimal <* skipSpace1
    case code of
        210 -> do
            -- consume message text
            _msg <- Atto.skipWhile (not . isEndOfLine) <* skipSpace
            entries <- manyTill (many skipComment *> parseEachCddbLine <* many skipComment) (string ".")
            return (code, Right entries)
        _ -> do
            body <- Atto.takeTill (const True)
            return (code, Left body)
  where
    skipComment = string "#" *> Atto.skipWhile (not . isEndOfLine) <* skipSpace
    parseEachCddbLine = do
        key <- Atto.takeWhile (/= '=')
        _ <- string "="
        value <- Atto.takeWhile (not . isEndOfLine) <* skipSpace1
        return (key, value)

parseResponse :: Parser (Int, Either T.Text a) -> T.Text -> Either FreeDBError a
parseResponse parser res =
    case parseOnly parser res of
        Left err -> Left $ ResponseParseError err res
        Right (code, Left err) -> Left $ ResponseStatusError code err
        Right (_code, Right result) -> Right result

parseFreeDBQueryResponse :: T.Text -> Either FreeDBError [(T.Text, T.Text, T.Text)]
parseFreeDBQueryResponse = parseResponse freeDBQueryResponseParser

parseFreeDBReadResponse :: T.Text -> Either FreeDBError [(T.Text, T.Text)]
parseFreeDBReadResponse = parseResponse freeDBReadResponseParser
