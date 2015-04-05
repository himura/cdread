{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include <linux/cdrom.h>
#include "cd-discid.h"

module System.CDROM
       where

import System.Posix.IO
import Foreign
import Foreign.C
import System.Posix.Types
import Control.Applicative
import Control.Monad
import Control.Monad.Catch

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt
foreign import ccall "read_tocentry" c_tocentry :: CInt -> CInt -> Ptr CUInt -> Ptr CUInt -> IO CInt

data TocHeader = TocHeader
    { tocHeaderStart :: Int
    , tocHeaderEnd :: Int
    } deriving (Eq, Show)
instance Storable TocHeader where
    sizeOf _ = {# sizeof cdrom_tochdr #}
    alignment _ = 4
    peek p = TocHeader
             <$> fmap fromIntegral ({# get cdrom_tochdr->cdth_trk0 #} p)
             <*> fmap fromIntegral ({# get cdrom_tochdr->cdth_trk1 #} p)
    poke p (TocHeader start end) = do
        {# set cdrom_tochdr.cdth_trk0 #} p (fromIntegral start)
        {# set cdrom_tochdr.cdth_trk1 #} p (fromIntegral end)
{# pointer *cdrom_tochdr as TocHeaderPtr -> TocHeader #}

read_toc_header :: CInt
read_toc_header = {#const CDROMREADTOCHDR#}

readTocHeader :: Fd -> IO TocHeader
readTocHeader fd =
    alloca $ \ptr -> do
        throwErrnoIfMinus1_ "ioctl" $ c_ioctl (fromIntegral fd) read_toc_header (castPtr ptr)
        peek ptr

withCdromFd :: FilePath -> (Fd -> IO a) -> IO a
withCdromFd dev inner =
    bracket (openFd dev ReadOnly Nothing (defaultFileFlags { nonBlock = True })) closeFd inner

readTocEntry :: Fd -> TocHeader -> IO (CUInt, [CUInt])
readTocEntry fd hdr@(TocHeader start end) = do
    allocaArray size $ \resultPtr ->
        alloca $ \diskIdPtr -> do
            last <- c_tocentry (fromIntegral fd) (fromIntegral end) diskIdPtr resultPtr
            diskId <- peek diskIdPtr
            trackOffset <- peekArray (fromIntegral last) resultPtr
            return (diskId, trackOffset)
  where
    size = end + 1
