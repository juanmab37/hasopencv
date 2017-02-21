module HOpenCV.CV.Util
where

import Foreign
import HOpenCV.CV.Error

withForeignPtr2 :: ForeignPtr a -> ForeignPtr b
                   -> (Ptr a -> Ptr b -> IO c)
                   -> IO c
withForeignPtr2 a b f
  = let g a' = withForeignPtr b $ f a'
    in withForeignPtr a g

withForeignPtr3 :: ForeignPtr a -> ForeignPtr b -> ForeignPtr c
                   -> (Ptr a -> Ptr b -> Ptr c -> IO d)
                   -> IO d
withForeignPtr3 a b c f
  = withForeignPtr a $ \a' ->
    withForeignPtr b $ \b' ->
    withForeignPtr c $ \c' ->
    f a' b' c'

withForeignPtr5 :: ForeignPtr a -> ForeignPtr b -> ForeignPtr c
                   -> ForeignPtr d -> ForeignPtr e
                   -> (Ptr a -> Ptr b -> Ptr c -> Ptr d -> Ptr e -> IO f)
                   -> IO f
withForeignPtr5 a b c d e f
  = withForeignPtr a $ \a' ->
    withForeignPtr b $ \b' ->
    withForeignPtr c $ \c' ->
    withForeignPtr d $ \d' ->
    withForeignPtr e $ \e' ->
    f a' b' c' d' e'

-- |Fail if an action results in a null pointer. 
-- NOTE: adapted from the allocated-processor package.
mycheckPtr_2 :: IO (Ptr a) -> IO (ErrCV (Ptr a))
mycheckPtr_2 x = do 
  res <- x
  if res /= nullPtr 
    then return (Return res)
    else return (throwErrorCV "Null Pointer")


