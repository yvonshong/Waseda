#include <iostream>  
#include <opencv2/core/core.hpp>  
#include <opencv2/highgui/highgui.hpp>  

using namespace cv;

int main() {
	// import a pic file  
	Mat img = imread("D:\\OneDrive\\ͼƬ\\MyerSplash\\pic.jpg");
	// build a new windows named "Picture"    
	namedWindow("Picture");
	// show the pic in the new windows
	imshow("Picture", img);
	// wait for 6000 ms, then close the windows    
	waitKey(12000);
}