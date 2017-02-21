{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TypeFamilies #-}

module HOpenCV.CV.CxCore where

import Foreign
import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.C.String
import Control.Monad ((>=>))
import Data.VectorSpace as VectorSpace

import HOpenCV.CV.Util
import HOpenCV.CV.Error

#include <opencv/cxcore.h>

------------------------------------------------------
toFromIntegral :: (RealFrac c, Integral b, Integral a, Num b1) => (b1 -> c) -> a -> b
toFromIntegral f = round . f . fromIntegral

toFromIntegral2 :: (Integral a, Num b, Integral a1, Num b1, RealFrac a2, Integral b2) => (b -> b1 -> a2) -> a -> a1 -> b2
toFromIntegral2 f x y = round (f (fromIntegral x) (fromIntegral y))
------------------------------------------------------

data CvSize  = CvSize { sizeWidth :: CInt, sizeHeight :: CInt }
               deriving (Show, Eq)

instance Storable CvSize where
    sizeOf    _ = (#size CvSize)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        w <- (#peek CvSize, width) ptr
        h <- (#peek CvSize, height) ptr
        return  (CvSize w h)
    poke ptr (CvSize w h) = do
        (#poke CvSize, width) ptr w
        (#poke CvSize, height) ptr h

liftCvSize ::(RealFrac c, Num b) => (b -> c) -> CvSize -> CvSize
liftCvSize f (CvSize w h) = CvSize (f' w) (f' h)
    where f' = toFromIntegral f

liftCvSize2 :: (Num b, Num b1, RealFrac a) => (b -> b1 -> a) -> CvSize -> CvSize -> CvSize
liftCvSize2 f (CvSize w1 h1) (CvSize w2 h2) = CvSize (f' w1 w2) (f' h1 h2)
    where f' = toFromIntegral2 f

instance AdditiveGroup CvSize where
  zeroV = CvSize 0 0
  (^+^) = liftCvSize2 (+)
  negateV = liftCvSize (0-)

instance VectorSpace CvSize where
  type Scalar CvSize = Double -- todo: use CInt instead of Double here?
  a *^ s = liftCvSize (a*) s

data CvRect  = CvRect { rectX :: CInt, rectY :: CInt, rectWidth :: CInt, rectHeight :: CInt }
               deriving (Show, Eq)

instance Storable CvRect where
    sizeOf    _ = (#size CvRect)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        x <- (#peek CvRect, x) ptr
        y <- (#peek CvRect, y) ptr
        w <- (#peek CvRect, width) ptr
        h <- (#peek CvRect, height) ptr
        return  (CvRect x y w h)
    poke ptr (CvRect x y w h) = do
        (#poke CvRect, x) ptr x
        (#poke CvRect, y) ptr y
        (#poke CvRect, width) ptr w
        (#poke CvRect, height) ptr h


liftCvRect :: (RealFrac c, Num b) => (b -> c) -> CvRect -> CvRect
liftCvRect f (CvRect x y w h) = CvRect (f' x) (f' y) (f' w) (f' h)
    where f' = toFromIntegral f

liftCvRect2 :: (Num b, Num b1, RealFrac a) => (b -> b1 -> a) -> CvRect -> CvRect -> CvRect
liftCvRect2 f (CvRect x1 y1 w1 h1) (CvRect x2 y2 w2 h2) = CvRect (f' x1 x2) (f' y1 y2) (f' w1 w2) (f' h1 h2)
    where f' = toFromIntegral2 f

instance AdditiveGroup CvRect where
  zeroV = CvRect 0 0 0 0
  (^+^) = liftCvRect2 (+)
  negateV = liftCvRect (0-)

instance VectorSpace CvRect where
  type Scalar CvRect = Double -- todo: use CInt instead of Double here?
  a *^ r = liftCvRect (a*) r

------------------------------------------------------
{-
data Priv_CvArr
type CvArr = ForeignPtr Priv_CvArr
-}

data Priv_IplImage
type IplImage = ForeignPtr Priv_IplImage

data Priv_CvMemStorage
type MemStorage = ForeignPtr Priv_CvMemStorage

data Priv_CvSeq a
type CvSeq a = ForeignPtr (Priv_CvSeq a)

newtype Depth = Depth { unDepth :: CInt }
    deriving (Eq, Show)

#{enum Depth, Depth
  , iplDepth1u = IPL_DEPTH_1U
  , iplDepth8u = IPL_DEPTH_8U
  , iplDepth8s = IPL_DEPTH_8S
  , iplDepth16u = IPL_DEPTH_16U
  , iplDepth16s = IPL_DEPTH_16S
  , iplDepth32s = IPL_DEPTH_32S
  , iplDepth32f = IPL_DEPTH_32F
  , iplDepth64f = IPL_DEPTH_64F
}

validDepths :: [Depth]
validDepths = [iplDepth1u, iplDepth8u, iplDepth8s, iplDepth16u, iplDepth16s, iplDepth32s, iplDepth32f, iplDepth64f]

depthsLookupList :: [(CInt, Depth)]
depthsLookupList = map (\d -> (unDepth d, d)) validDepths

numToDepth :: CInt -> Maybe Depth
numToDepth x = lookup x depthsLookupList

---------------------------------------------------------------
-- mem storage

foreign import ccall unsafe "opencv/cxcore.h cvCreateMemStorage"
  c_cvCreateMemStorage :: CInt -> IO (Ptr Priv_CvMemStorage)

foreign import ccall unsafe "HOpenCV_wrap.h &release_mem_storage"
  cf_releaseMemStorage :: FunPtr (Ptr Priv_CvMemStorage -> IO ())

createMemStorage :: Int -> IO (ErrCV MemStorage)
createMemStorage i
  = do p <- mycheckPtr_2 . c_cvCreateMemStorage $ fromIntegral i
       case p of
            (Return p') -> do
                            ptr <- newForeignPtr cf_releaseMemStorage p'
                            return (Return ptr)
            (Raise s) -> return( throwErrorCV "Failed to create mem storage" )


---------------------------------------------------------------
-- images / matrices / arrays

foreign import ccall unsafe "HOpenCV_wrap.h &cv_free"
  cvFree :: FunPtr (Ptr a -> IO ())

foreign import ccall unsafe "HOpenCV_wrap.h create_image"
  c_cvCreateImage :: CInt -> CInt -> CInt -> CInt -> IO (Ptr Priv_IplImage)

createImage :: CvSize -> Depth -> Int -> IO (ErrCV IplImage)
createImage size depth numChans
  = do im <- mycheckPtr_2 $ c_cvCreateImage (sizeWidth size) (sizeHeight size)
                                            (unDepth depth)
                                            (fromIntegral numChans)
       case im of
            (Return i) -> do
                               fp <- newForeignPtr cvFree i
                               return (Return fp)
            (Raise s) -> return( throwErrorCV "Failed to create image" )


foreign import ccall "HOpenCV_wrap.h &release_image"
  cp_release_image :: FunPtr (Ptr IplImage -> IO ())


foreign import ccall unsafe "opencv/cxcore.h cvCloneImage"
  c_cvCloneImage :: Ptr Priv_IplImage -> IO (Ptr Priv_IplImage)

cloneImage :: IplImage -> IO (ErrCV IplImage)
cloneImage p
  = do p' <- mycheckPtr_2 $ withForeignPtr p c_cvCloneImage
       case p' of
            (Return i) -> do
                               fp <- newForeignPtr cvFree i
                               return (Return fp)
            (Raise s) -> return( throwErrorCV "Failed to clone image" )


foreign import ccall unsafe "HOpenCV_wrap.h get_size"
  c_get_size :: Ptr Priv_IplImage -> Ptr CvSize -> IO ()

foreign import ccall unsafe "opencv/cxcore.h cvCopy"
  c_cvCopy :: Ptr Priv_IplImage -> Ptr Priv_IplImage -> Ptr Priv_IplImage -> IO ()

-- todo add mask support
copy :: IplImage -> IplImage -> IO ()
copy src dst
  = withForeignPtr2 src dst $ \s d ->
    c_cvCopy s d nullPtr

foreign import ccall unsafe "opencv/cxcore.h cvMerge"
  cvMerge :: Ptr Priv_IplImage -> Ptr Priv_IplImage -> Ptr Priv_IplImage
             -> Ptr Priv_IplImage -> Ptr Priv_IplImage -> IO ()

merge :: IplImage -> IplImage -> IplImage -> IplImage -> IplImage -> IO ()
merge a b c d e
  = withForeignPtr5 a b c d e $ \a' b' c' d' e' ->
     cvMerge a' b' c' d' e'

foreign import ccall unsafe "HOpenCV_wrap.h wrap_getImageData"
  wrap_getImageData :: Ptr Priv_IplImage -> IO (Ptr CUChar)

getImageData :: IplImage -> IO (ErrCV (Ptr CUChar))
getImageData i
  = do im <- mycheckPtr_2 $ withForeignPtr i wrap_getImageData
       case im of
            (Return im') -> return (Return im')
            (Raise s) -> return( throwErrorCV "Failed to getImageData" )

getSize :: IplImage -> IO (ErrCV CvSize)
getSize a
  = alloca $ \cvSizePtr -> do
      withForeignPtr a $ \a' -> c_get_size a' cvSizePtr
      sz <- mycheckPtr_2 $ inout cvSizePtr
      case sz of
            (Return sz') -> do
                              size <- peek sz'
                              return (Return size)
            (Raise s) -> return( throwErrorCV "Failed to getSize" )

foreign import ccall unsafe "HOpenCV_wrap.h get_depth"
  c_get_depth :: Ptr Priv_IplImage -> IO CInt

getDepth :: IplImage -> IO (ErrCV Depth)
getDepth img = do
  depthInt <- withForeignPtr img c_get_depth
  case numToDepth depthInt of
    Nothing -> return( throwErrorCV "Bad depth in image struct")
    Just depth -> return (Return depth)

foreign import ccall unsafe "HOpenCV_wrap.h get_nChannels"
  c_get_nChannels :: Ptr Priv_IplImage -> IO CInt

getNumChannels :: Integral a => IplImage -> IO (ErrCV a)
getNumChannels img
  = do i <- withForeignPtr img c_get_nChannels
       if (i == 0)
            then return (throwErrorCV "Failed to getNumChannels")
            else return (Return (fromIntegral i))
 
 

foreign import ccall unsafe "HOpenCV_wrap.h wrap_getWidthStep"
  wrap_getWidthStep :: Ptr Priv_IplImage -> IO CInt

getWidthStep :: IplImage -> IO (ErrCV Int)
getWidthStep im
  = do i <- withForeignPtr im wrap_getWidthStep
       if (i == 0)
            then return (throwErrorCV "Failed to getWidthStep")
            else return (Return (fromIntegral i))


foreign import ccall unsafe "opencv/cxcore.h cvConvertScale"
  cvConvertScale :: Ptr Priv_IplImage -> Ptr Priv_IplImage -> CDouble -> CDouble -> IO ()

convertScale :: IplImage -> IplImage -> Double -> Double -> IO ()
convertScale a b c d
  = withForeignPtr2 a b $ \a' b' ->
     cvConvertScale a' b'
                    (realToFrac c)
                    (realToFrac d)

foreign import ccall unsafe "opencv/cxcore.h cvLoad"
  c_cvLoad :: CString -> Ptr Priv_CvMemStorage -> CString -> Ptr CString -> IO (Ptr a)

load :: String -> MemStorage -> Maybe String -> IO (ErrCV (ForeignPtr a, Maybe String))
load filename mem name
  = withCString filename $ \filenameC ->
    case name
      of Nothing -> cvLoad'' filenameC nullPtr
         Just n' -> withCString n' $ cvLoad'' filenameC
 where
  cvLoad'' filenameC nameC
    = alloca $ \ptrRealNameC ->
        do g <- withForeignPtr mem $ \mem' -> mycheckPtr_2 $ c_cvLoad filenameC mem' nameC ptrRealNameC
           case g of
            (Return ptrObj) -> do
                                realNameC <- peek ptrRealNameC
                                realName <- if realNameC == nullPtr
                                             then return Nothing
                                             else fmap Just $ peekCString realNameC
                                -- cvFree realNameC
                                fp <- newForeignPtr cvFree ptrObj
                                return (Return (fp, realName))
            (Raise s) -> return( throwErrorCV ("cvLoad failed (" ++ s ++ ")") )
           

foreign import ccall unsafe "opencv/cxcore.h cvGetSeqElem"
  cvGetSeqElem :: Ptr (Priv_CvSeq a) -> CInt -> IO (Ptr a)

-- foreign import ccall unsafe "HOpenCV_wrap.h c_rect_cvGetSeqElem"
--   cvGetSeqElemRect :: Ptr (CvSeq (Ptr CvRect)) -> CInt -> IO (Ptr CvRect)

foreign import ccall unsafe "HOpenCV_wrap.h seq_total"
  seqNumElems :: Ptr (Priv_CvSeq a) -> IO CInt

seqToPList :: CvSeq a -> IO [ErrCV (ForeignPtr a)]
seqToPList pseq = do
  numElems <- withForeignPtr pseq seqNumElems
  mapM fetchElem [1..numElems]
 where
  fetchElem i
    = do p <- withForeignPtr pseq
              $ \p -> mycheckPtr_2 $ cvGetSeqElem p i
         case p of
            (Return p') -> do
                               fp <- newForeignPtr cvFree p'
                               return (Return fp)
            (Raise s) -> return( throwErrorCV ("Failed to cvGetSeqElem in seqToPList (" ++ s ++ ")") )
         

seqToList :: Storable a => CvSeq a -> IO [ErrCV a]
seqToList pseq = do
  numElems <- withForeignPtr pseq seqNumElems
  flip mapM [1..(numElems)] $ \i -> do
    elemP <- withForeignPtr pseq $ \p ->  mycheckPtr_2 $ cvGetSeqElem p i
    case elemP of
            (Return elemP') -> do
                                elem' <- peek elemP'
                                return (Return elem')
            (Raise s) -> return( throwErrorCV ("Failed to cvGetSeqElem in seqToList (" ++ s ++ ")") )


-- seqToRectList :: Ptr (CvSeq (Ptr CvRect)) -> IO [CvRect]
-- seqToRectList pseq = do
--   numElems <- withForeignPtr pseq seqNumElems
--   flip mapM [1..(numElems)] $ \i -> do
--     rectP <- cvGetSeqElemRect pseq i
--     rect <- peek rectP
--     return rect

foreign import ccall unsafe "HOpenCV_wrap.h c_cvRectangle"
  c_cvRectangle :: Ptr Priv_IplImage -> CInt -> CInt -> CInt -> CInt -> IO ()

rectangle :: IplImage -> CvRect -> IO ()
rectangle dst (CvRect x y w h)
  = withForeignPtr dst $ \d ->
    c_cvRectangle d x y w h

------------------------------------------------------------------------------
-- Debugging stuff, not part of opencv

-- | Debugging function to print some of the internal details of an IplImage structure
foreign import ccall unsafe "HOpenCV_wrap.h debug_print_image_header"
  c_debug_print_image_header :: Ptr Priv_IplImage -> IO () 

-- |Fail if an action results in a null pointer. 
-- NOTE: adapted from the allocated-processor package.
mycheckPtr :: IO (Ptr a) -> IO (Ptr a)
mycheckPtr = (>>= aux)
  where aux r | r == nullPtr = fail "Null Pointer"
              | otherwise = return r

-- |Wrap a 'ForeignPtr' around a 'Ptr' after checking that the 'Ptr'
-- is non-null. The supplied finalizer is attached to the
-- 'ForeignPtr'.
-- NOTE: adapted from the allocated-processor package.
createForeignPtr :: FunPtr (Ptr a -> IO ()) -> IO (Ptr a) -> IO (ForeignPtr a)
createForeignPtr = (checkPtr >=>) . newForeignPtr
