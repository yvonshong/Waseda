# Video Signal Processing - OpenCV Report
Authored by SONG DAIWEI   #44161588-3

## configuring the environment
OpenCV 3.1 + Visual Studio 2015

when using OpenCV to process image and debug, there will be some error about running out of memory or crashing. It results from the path of image. We should use "D:\\\\pic.jpg" instead of "D:\\pic.jpg".

And we can import the [.props file](DebugPropertySheet.props) to configure the environment of OpenCV in Visual Studio 2015 of Debug Mode.

