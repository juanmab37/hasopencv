import Control.Monad 
import HOpenCV.OpenCV

main :: IO ()
main = do
    img <- loadImage "data/Lenna.png" loadImageGrayscale         
    namedWindow "test" autoSize 
    showImage "test" img
    waitKey 10000
    destroyWindow "test"
