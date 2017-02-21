module Main where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

import HOpenCV.OpenCV

import Control.Monad


main :: IO ()
main = do 
        putStrLn "Running..."
        let win = "win"
        namedWindow win autoSize
        img <- loadImage "1.png" loadImageColor
        case img of
                (Return i) -> do
                                showImage win i
                                key <- waitKey 5000
                                r <- saveImage "new.png" i
                                print r
                                destroyWindow win
                                
                (Raise s) -> print s
      
         
