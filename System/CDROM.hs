module System.CDROM
       ( Toc (..)
       , readToc
       ) where

import System.Posix.IO
import System.Posix.Types
import Control.Monad.Catch
import System.CDROM.Internal

data Toc = Toc
    { tocDiscId :: Integer
    , tocAddresses :: [Integer]
    , tocLengthOfDiscInSec :: Integer
    } deriving (Show, Eq)

withCdromFd :: FilePath -> (Fd -> IO a) -> IO a
withCdromFd dev inner =
    bracket (openFd dev ReadOnly Nothing (defaultFileFlags { nonBlock = True })) closeFd inner

readToc :: FilePath -> IO Toc
readToc dev =
    withCdromFd dev $ \fd -> do
        hdr <- readTocHeader fd
        (discid, addresses, discLen) <- readTocEntry fd hdr
        return $ Toc (fromIntegral discid) (map fromIntegral addresses) (fromIntegral discLen)
