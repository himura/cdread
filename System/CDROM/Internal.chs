{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include <linux/cdrom.h>
#include "cd-discid.h"

module System.CDROM.Internal
       where

import Foreign
import Foreign.C
import System.Posix.Types
import Control.Applicative

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

read_toc_header :: CInt
read_toc_header = {#const CDROMREADTOCHDR#}

readTocHeader :: Fd -> IO TocHeader
readTocHeader fd =
    alloca $ \ptr -> do
        throwErrnoIfMinus1_ "ioctl" $ c_ioctl (fromIntegral fd) read_toc_header (castPtr ptr)
        peek ptr

readTocEntry :: Fd -> TocHeader -> IO (CUInt, [CUInt], CUInt)
readTocEntry fd (TocHeader _start end) = do
    allocaArray size $ \resultPtr ->
        alloca $ \diskIdPtr -> do
            lastTrack <- fromIntegral <$> c_tocentry (fromIntegral fd) (fromIntegral end) diskIdPtr resultPtr
            diskId <- peek diskIdPtr
            trackOffset <- peekArray lastTrack resultPtr
            lengthOfDiscInSec <- peekElemOff resultPtr lastTrack
            return (diskId, trackOffset, lengthOfDiscInSec)
  where
    size = end + 1
