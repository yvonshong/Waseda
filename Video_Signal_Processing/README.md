# Video Signal Processing - OpenCV Report
Authored by SONG DAIWEI   #44161588-3

## Configuring the environment
[OpenCV 3.1](http://opencv.org/) + Visual Studio 2015

And we unzip it to C:/opencv

### Add to system environment variables
This computer>>right click>>Properties>>Advanced system settings>>Environment Variables...>>Path>>Edit>>New>>add C:/opencv/build/x64/vc14/bin

### Configure the VS 2015
1. Build a Windows ConsoleApplication project
1. Be sure about the **Debug Mode** AND **x64**
1. Then open the PropertySheet properties page in VS2015
VC++ Directory>>Include Directory
    ```
    C:\opencv\build\include;
    C:\opencv\build\include\opencv;
    C:\opencv\build\include\opencv2;
    ```

1. VC++ Directory>>Library Directory
    
    ```
    C:\opencv\build\x64\vc14\lib;
    ```

1. Linker>>input>>Additional Dependency
    ```
    opencv_world310d.lib
    ```
    When in **Release Mode** we should add 
    ```
    opencv_world310.lib
    ```



### Import the PopertySheet file

Also we can import the [DebugPropertySheet.props](DebugPropertySheet.props) to configure the environment of OpenCV in Visual Studio 2015 of Debug Mode.

[ReleasePropertySheet.props](ReleasePropertySheet.props) for Release Mode


## Code
When using OpenCV to process image and debug, there will be some error about running out of memory or crashing. It results from the path of image. We should use 
```
"D:\\\\pic.jpg"
```
instead of 
```
"D:\\pic.jpg"
```

