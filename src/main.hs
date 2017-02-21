module Main where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

import HOpenCV.OpenCV

import Control.Monad
import Data.Maybe (isNothing)

showFrames :: String -> IplImage -> Capture -> IO ()
showFrames win target cap  = do
  let step 0 = return ()
      step n = do
        frame <- queryFrame cap
        case frame of
                (Return f) -> do
                                convertImage f target 0
                                canny target target 30 190 3
                                showImage win target
                                k <- waitKey 5
                                case k of
                                    (Return c) -> putStr $ "\r" ++ show n ++ "\t\t frames before exiting...\r"
                                    (Raise s) ->  step (n - 1)
                (Raise s) -> print s
  step 1000



main :: IO ()
main = do 
        putStrLn "Running..."
        let win = "win"
        namedWindow win autoSize
        cap <- createCameraCapture 0
        case cap of
                (Return c) -> do
                                print "Adentro"
                                frame  <- queryFrame c
                                case frame of
                                        (Return f) -> do
                                                        size   <- getSize f
                                                        case size of
                                                            (Return size') -> do
                                                                                  print ("Size (" ++ show (sizeWidth size') ++ "," ++ show (sizeHeight size') ++ ")\n")
                                                                                  target <- createImage size' iplDepth8u 1
                                                                                  case target of
                                                                                    (Return t) -> showFrames win t c
                                                                                    (Raise s) -> print s  
                                                            (Raise s) -> print s
                                                        
                                        (Raise s) -> print s
                (Raise s) -> print s 



{-      img <- loadImage "1.png" loadImageColor
        case img of
                (Return i) -> do
                                showImage win i
                                key <- waitKey 5000
                                r <- saveImage "new.png" i
                                print r
                                destroyWindow win
                                
                (Raise s) -> print s
-}        
         
