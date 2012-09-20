{-# LANGUAGE ForeignFunctionInterface,OverloadedStrings #-}
module Text.Markdown.Discount (markdown, markdownIO) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as BUS
import           System.IO.Unsafe (unsafePerformIO)
import           Foreign (free)
import           Foreign.C
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.Storable (peek)
import           Foreign.C.String (CString)

type MMIOT = Ptr ()

foreign import ccall "mkdio.h mkd_string" c_mkd_string :: CString -> CInt -> CInt -> IO MMIOT
foreign import ccall "mkdio.h mkd_compile" c_mkd_compile :: MMIOT -> CInt -> IO CInt
foreign import ccall "mkdio.h mkd_document" c_mkd_document :: MMIOT -> Ptr CString -> IO CInt
foreign import ccall "mkdio.h mkd_cleanup" c_mkd_cleanup :: MMIOT -> IO ()

markdownIO :: B.ByteString -> IO B.ByteString
markdownIO md = BUS.unsafeUseAsCString md run
  where
    run inp = do
        mmiot <- c_mkd_string inp (CInt $ fromIntegral $ B.length md) (CInt 0)
        compres <- c_mkd_compile mmiot (CInt 0)
        case compres of
            CInt 1 -> do
                buf <- mallocForeignPtr
                out <- withForeignPtr buf (\p-> do
                    CInt len <- c_mkd_document mmiot p
                    cs <- peek p
                    f <- B.packCStringLen (cs, fromIntegral len)
                    return f
                    )
                c_mkd_cleanup mmiot
                return out
            CInt x -> c_mkd_cleanup mmiot >> error "compilation error on markdown!?"

markdown :: B.ByteString -> B.ByteString
markdown md = unsafePerformIO (markdownIO md)
