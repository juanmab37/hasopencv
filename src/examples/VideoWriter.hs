-- | Save video from an attached webcam to compressed video on disk
-- while also showing it on-screen.
import HOpenCV.OpenCV
import System.Exit (exitSuccess)

main :: IO ()
main = do cam <- createCameraCapture 0  
          --size <- (CvSize 640 480)
          writeImg <- createVideoWriter "/home/barufa/foo.mp4" (toFourCC "XVID") 20 (CvSize 640 480)
          --img <- createImage (CvSize 640 480) iplDepth8u 3
          --print writeImg
          namedWindow "Video Test" autoSize
          let kb 27 = exitSuccess
              kb  _ = go
              go = do frame <- queryFrame cam
                      --convertImage frame img 0
                      showImage "Video Test" frame
                      cp_release_capture cam
                      i <- writeFrame writeImg frame 	
                      print i
                      waitKey 1 >>= maybe go kb
          go
          releaseVideoWriter writeImg
