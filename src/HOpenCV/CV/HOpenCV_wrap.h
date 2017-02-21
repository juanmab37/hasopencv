#include <opencv/cv.h>
#include <opencv/cxcore.h>
#include <opencv/highgui.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

	extern void debug_print_image_header(IplImage *image);
	extern void release_capture(CvCapture *capture);
	extern void new_window(int num, int flags);
	extern void del_window(int num);
	extern void show_image(int num, IplImage *image);

	extern IplImage *create_image(int width, int height, int depth, int channels);
	extern void release_image(IplImage *image);
	extern char *wrap_getImageData(const IplImage *im);
	extern int wrap_getWidthStep(IplImage *im);
	
	extern void get_size(const CvArr *arr, CvSize *size);
	extern int get_depth(const IplImage *image);
	extern int get_nChannels(const IplImage *image);

	extern void dilate(const CvArr *src, CvArr *dest, int iterations);

	extern void release_mem_storage(CvMemStorage *mem_store);
	extern void cv_free(void *obj);

	extern int seq_total(const CvSeq *seq);
	/* extern  CvRect *c_rect_cvGetSeqElem(const CvSeq *seq, int index); */

	extern void c_cvRectangle(CvArr *img, int x, int y, int width, int height);


	extern CvSeq *c_cvHaarDetectObjects( const CvArr* image,
								  CvHaarClassifierCascade* cascade,
								  CvMemStorage* storage, double scale_factor,
								  int min_neighbors , int flags,
								  int width, int height);

	/*extern int wrap_createTrackbar( const char* trackbarName,
							 const char* winName,
							 int*        startPos,
							 int         maxVal,
							 CvTrackbarCallback onChange );*/
							
	extern int wrap_createTrackbar( const char* trackbarName
                       , const char* winName
                       , int*        startPos
                       , int         maxVal);

	extern CvVideoWriter *
	wrap_cvCreateVideoWriter( const char *file
							, int fourcc
							, double fps
							, int width
							, int height);
							
	extern int wrap_cvWriteFrame(CvVideoWriter* writer, const IplImage* image);
							
#ifdef __cplusplus
}
#endif


