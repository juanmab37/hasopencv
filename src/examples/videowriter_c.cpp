#include <opencv/cxcore.h>
#include <opencv/cv.h>
#include <opencv/highgui.h>
#include <stdio.h>


int main(int argc, char** argv)
{
    //IplImage* motion = 0;
    CvCapture* capture = 0;

    if( argc == 1 || (argc == 2 && strlen(argv[1]) == 1 && isdigit(argv[1][0])))
        capture = cvCaptureFromCAM( argc == 2 ? argv[1][0] - '0' : 0 );
    else if( argc == 2 )
        capture = cvCaptureFromFile( argv[1] );

    CvVideoWriter *writer = cvCreateVideoWriter("/home/barufa/video_ex.mp4", CV_FOURCC('X', 'V', 'I', 'D'), 20, cvSize(640, 480));
        assert(writer!=0);
	printf("Return %d\n",writer);
	
    if( capture )
    {
        cvNamedWindow( "Motion", 1 );

        for(;;)
        {
            IplImage* image = cvQueryFrame( capture );
            if( !image )
                break;

              int ret = cvWriteFrame(writer, image);
			  printf("Return %d\n",ret);
         cvShowImage( "Motion", image );

            if( cvWaitKey(10) >= 0 )
                break;
        }
        cvReleaseCapture( &capture );
        cvDestroyWindow( "Motion" );
        cvReleaseVideoWriter(&writer);
    }

    return 0;
}


