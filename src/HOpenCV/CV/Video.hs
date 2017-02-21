{-# Language ForeignFunctionInterface, EmptyDataDecls #-}

module HOpenCV.CV.Video
where

import Foreign
import Foreign.C
import HOpenCV.CV.CxCore
import HOpenCV.CV.Util
import HOpenCV.CV.Error

data Priv_CvVideoWriter

type CvVideoWriter = Ptr Priv_CvVideoWriter

foreign import ccall unsafe "HOpenCV_wrap.h wrap_cvCreateVideoWriter"
  wrap_cvCreateVideoWriter :: CString -> CInt -> CDouble -> CInt -> CInt -> IO (Ptr Priv_CvVideoWriter)

--type FourCC = String
type FourCC = (Char, Char, Char, Char)

fourCC :: FourCC -> CInt
fourCC (a,b,c,d) = (c1 .&. 255) + shiftL (c2 .&. 255) 8 + 
                   shiftL (c3 .&. 255) 16 + shiftL (c4 .&. 255) 24
    where [c1,c2,c3,c4] = map (fromIntegral . fromEnum) [a,b,c,d]

-- | Parse a four-character 'String' into a 'FourCC' code (e.g. "XVID").
toFourCC :: String -> FourCC
toFourCC [a,b,c,d] = (a,b,c,d)
toFourCC c = error $ "Invalid FourCC code: "++c

mpeg4CC :: FourCC
mpeg4CC = ('F','M','P','4')

createVideoWriter :: String -> FourCC -> Double -> CvSize -> IO (ErrCV CvVideoWriter)
createVideoWriter file codec fps size -- = withCString file $ \f  ->
                                      --      wrap_cvCreateVideoWriter f (fourCC codec) (realToFrac fps) (sizeWidth size) (sizeHeight size)
 = do ret <- mycheckPtr_2 $ withCString file
            $ \f -> wrap_cvCreateVideoWriter f (fourCC codec) (realToFrac fps) (sizeWidth size) (sizeHeight size)
      case ret of
        (Return x) -> return ret
        (Raise s) -> return (throwErrorCV ("Failed to createVideoWriter from file: '" ++ file ++ "' (" ++ s ++ ")") )

foreign import ccall unsafe "opencv/highgui.h cvReleaseVideoWriter"
  releaseVideoWriter :: CvVideoWriter -> IO ()

foreign import ccall unsafe "HOpenCV_wrap.h wrap_cvWriteFrame" --"opencv/highgui.h cvWriteFrame"
  cvWriteFrame :: Ptr Priv_CvVideoWriter -> Ptr Priv_IplImage -> IO CInt

writeFrame :: CvVideoWriter -> IplImage -> IO (ErrCV Int)
writeFrame vw im
  = do i <- withForeignPtr im $ \ptr_im -> 
            cvWriteFrame vw ptr_im
       if (i == -1) 
            then return (throwErrorCV ("Failed to cvWriteFrame = -1") )
            else return (Return (fromIntegral i))         


{--ptr_img <- withForeignPtr im -- createForeignPtr cp_release_image im 
cloneImage p
  = do p' <- errorName "Failed to clone image" . checkPtr
             $ withForeignPtr p c_cvCloneImage
       fp <- newForeignPtr cvFree p'
       return fp--}

