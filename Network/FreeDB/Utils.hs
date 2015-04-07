{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.FreeDB.Utils
       where

import Control.Applicative
import Data.Attoparsec.Text as Atto
import qualified Data.Text as T

parseTitleAuthor :: T.Text -> (T.Text, Maybe T.Text)
parseTitleAuthor title =
    case parseOnly titleAuthorParser title of
        Left _err -> (title, Nothing)
        Right result -> result

titleAuthorParser :: Parser (T.Text, Maybe T.Text)
titleAuthorParser = do
    author <- (Just . T.pack <$> authorParser) <|> return Nothing
    title <- Atto.takeText
    return (title, author)
  where
    authorParser = Atto.manyTill Atto.anyChar (skipSpace1 *> Atto.satisfy (Atto.inClass "/-") <* skipSpace1)
    skipSpace1 = Atto.takeWhile1 isHorizontalSpace >> return ()
