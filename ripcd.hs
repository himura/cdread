{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Conduit.Process()
import Data.Conduit.Text
import Data.Maybe
import Data.Streaming.Process
import qualified Data.Text as T
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT
import System.Process as Process
import System.CDROM
import Text.Printf
import Data.List

data DiskInfo = DiskInfo

data Track = Track Integer Integer Integer
           deriving (Show, Eq)

diskInfoParser :: Parser [Track]
diskInfoParser = do
    catMaybes <$> many parseLine
  where
    parseLine = do
        (Just <$> trackParser) <|> (manyTill anyChar endOfLine *> return Nothing)

trackParser :: Parser Track
trackParser = do
    trackNumber <- skipSpace *> decimal <* string "."
    trackLength <- skipSpace *> decimal <* skipSpace
    skipHumanReadableTime
    trackBegin <- skipSpace *> decimal <* skipSpace
    skipHumanReadableTime
    manyTill anyChar endOfLine

    return $ Track trackNumber trackLength trackBegin

  where
    skipHumanReadableTime = string "[" *> manyTill anyChar (string "]") >> return ()

obtainDiskInfo :: FilePath
               -> IO [Track]
obtainDiskInfo dev = do
    (ClosedStream, ClosedStream, (stderrSource, close), processHandle) <- streamingProcess (Process.proc "cdparanoia" ["-d", dev, "-vQ"])
    res <- stderrSource $= decodeUtf8 $$ sinkParser diskInfoParser
    close
    _ec <- waitForStreamingProcess processHandle
    return res

