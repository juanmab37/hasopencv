{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module HOpenCV.CV.HighGui where

import Control.Monad
import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable

import HOpenCV.CV.CxCore
import HOpenCV.CV.Util
import HOpenCV.CV.Error

#include <opencv/highgui.h>

------------------------------------------------
-- General
foreign import ccall unsafe "opencv/highgui.h cvConvertImage"
  c_cvConvertImage :: Ptr Priv_IplImage -> Ptr Priv_IplImage -> CInt -> IO ()

convertImage :: IplImage -> IplImage -> Int -> IO ()
convertImage src dst flags
  = withForeignPtr2 src dst
     $ \s d -> c_cvConvertImage s d
                                (fromIntegral flags)

------------------------------------------------
-- Capturing
data Priv_CvCapture
type Capture = ForeignPtr Priv_CvCapture


foreign import ccall unsafe "opencv/highgui.h cvCreateCameraCapture"
  c_cvCreateCameraCapture :: CInt -> IO (Ptr Priv_CvCapture)

-- | self-documenting camera specification
pickAnyCam :: Int
pickAnyCam = -1

-- | self-documenting camera specification
cam :: Int -> Int
cam = id 

createCameraCapture :: Int -> IO (ErrCV Capture)
createCameraCapture x
  = do p <- mycheckPtr_2 $ c_cvCreateCameraCapture . fromIntegral $ x
       case p of
            (Return p') -> do
                            ptr <- newForeignPtr cp_release_capture p'
                            return (Return ptr)
            (Raise s) -> return( throwErrorCV ("Failed to create camera (" ++ s ++ ")") )


foreign import ccall unsafe "opencv/highgui.h cvCreateFileCapture"
  c_cvCreateFileCapture :: CString -> IO (Ptr Priv_CvCapture)

createFileCapture :: String -> IO (ErrCV Capture)
createFileCapture filename
  = do c <- mycheckPtr_2 $ withCString filename f
       case c of
            (Return c') -> do
                            ptr <- newForeignPtr cp_release_capture c'
                            return (Return ptr)
            (Raise s) -> return( throwErrorCV ("Failed to capture from file: '" ++ filename ++ "' (" ++ s ++ ")") )
    where f filenameC = c_cvCreateFileCapture filenameC

foreign import ccall unsafe "HOpenCV_wrap.h &release_capture"
  cp_release_capture  :: FunPtr (Ptr Priv_CvCapture -> IO ())

foreign import ccall unsafe "opencv/highgui.h cvQueryFrame"
  c_cvQueryFrame :: Ptr Priv_CvCapture -> IO (Ptr Priv_IplImage)

queryFrame :: Capture -> IO (ErrCV IplImage)
queryFrame cap
  = do i <- withForeignPtr cap $ \c -> mycheckPtr_2 $ c_cvQueryFrame c
       case i of
            (Return i') -> do
                               fp <- newForeignPtr_ i' -- no free! OpenCV demands queryFrame results not be freed by user.
                               return (Return fp)
            (Raise s) -> return( throwErrorCV ("Failed to query frame from camera (" ++ s ++ ")") )

-------------------------------------------------
-- Windows

foreign import ccall unsafe "opencv/highgui.h cvNamedWindow"
  cvNamedWindow :: CString -> CInt -> IO CInt

type AutoSize = Bool

-- | self-documenting window sizing specification
autoSize :: AutoSize
autoSize   = True

namedWindow :: String -> AutoSize -> IO ()
namedWindow s a
  = withCString s $ \cs ->
      do _ <- cvNamedWindow cs (fromIntegral $ fromEnum a)
         return ()

foreign import ccall unsafe "opencv/highgui.h cvDestroyWindow"
  cvDestroyWindow :: CString -> IO ()

destroyWindow :: String -> IO ()
destroyWindow wId
  = withCString wId cvDestroyWindow

foreign import ccall unsafe "opencv/highgui.h cvShowImage"
  cvShowImage :: CString -> Ptr Priv_IplImage -> IO ()

showImage :: String -> IplImage -> IO ()
showImage wId p
 = withCString wId $ \w ->
    withForeignPtr p $ cvShowImage w

foreign import ccall unsafe "opencv/highgui.h cvWaitKey"
  cvWaitKey :: CInt -> IO CInt

waitKey :: Int -> IO (ErrCV Int)
waitKey milliSecs
  = do i <- cvWaitKey $ fromIntegral milliSecs
       if i == (-1)
         then return (throwErrorCV ("No Key press..."))
         else return (Return (fromIntegral i))

newtype LoadImageColor = LoadImageColor { unLoadImageColor :: CInt }

#{enum LoadImageColor, LoadImageColor
    , loadImageColor     = CV_LOAD_IMAGE_COLOR
    , loadImageGrayscale = CV_LOAD_IMAGE_GRAYSCALE
    , loadImageUnchanged = CV_LOAD_IMAGE_UNCHANGED }

foreign import ccall unsafe "opencv/highgui.h cvLoadImage"
  c_cvLoadImage :: CString -> CInt -> IO (Ptr Priv_IplImage)

loadImage :: String -> LoadImageColor -> IO (ErrCV IplImage)
loadImage filename (LoadImageColor color) 
  = do i <- mycheckPtr_2 $ withCString filename
            $ \fn -> c_cvLoadImage fn color
       case i of
        (Return x) -> do 
                        fp <- newForeignPtr cvFree (runErr i)
                        return (Return fp)
        (Raise s) -> return (throwErrorCV ("Failed to load from file: '" ++ filename ++ "' (" ++ s ++ ")") )


foreign import ccall unsafe "opencv/highgui.h cvSaveImage"
  c_cvSaveImage :: CString -> Ptr Priv_IplImage -> IO CInt

saveImage :: String -> IplImage -> IO (ErrCV Int)
saveImage filename image = withCString filename f
  where
    f filenameC = do
      ret <- withForeignPtr image $ \i -> 
             c_cvSaveImage filenameC i
      if (ret == 0) 
        then return (throwErrorCV ("Failed to save to file: '" ++ filename ++ "'") )
        else return (Return (fromIntegral ret))

------------------------------------------------
-- Trackbar

foreign import ccall unsafe "HOpenCV_Wrap.h wrap_createTrackbar"
  wrap_createTrackbar :: CString -> CString -> Ptr CInt -> CInt -> IO ()

createTrackbar :: String -> String -> Maybe Int -> Int -> IO ()
createTrackbar trackbarName winName startPosition maxValue
  = withCString trackbarName $ \tb ->
    withCString winName      $ \wn ->
    alloca                   $ \sp ->
      do maybeToPtr sp startPosition
         wrap_createTrackbar tb wn sp (fromIntegral maxValue)
 where
  maybeToPtr mem (Just i) = poke mem (fromIntegral i)
  maybeToPtr mem Nothing  = poke mem (fromIntegral 0)

foreign import ccall unsafe "opencv/highgui.h cvGetTrackbarPos"
  cvGetTrackbarPos :: CString -> CString -> IO CInt

getTrackbarPos :: String -> String -> IO (ErrCV Int)
getTrackbarPos trackbarName winName
  = withCString trackbarName $ \tb ->
    withCString winName      $ \wn ->
      do i <- cvGetTrackbarPos tb wn
         if (i == 0) 
            then return (throwErrorCV ("Failed to getTrackbarPos to: '" ++ trackbarName ++ "' in winName: '"++ winName ++ "'") )
            else return (Return (fromIntegral i))         


foreign import ccall unsafe "opencv/highgui.h cvSetTrackbarPos"
  cvSetTrackbarPos :: CString -> CString -> CInt -> IO ()

setTrackbarPos :: String -> String -> Int -> IO ()
setTrackbarPos trackbarName winName pos
  = withCString trackbarName $ \tb ->
    withCString winName      $ \wn ->
      cvSetTrackbarPos tb wn (fromIntegral pos)
