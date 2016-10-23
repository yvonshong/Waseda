# Topic
Road Recognition and Lane Detection
# Background
In recent year, many factories and the drivers have to install a variety of driver assistance systems in vehicles, in order to reduce the accident rate. In various systems, auxiliary driving system based on image accounted for the highest rate. The main reason is the low cost, and it can be used with the driving recorder, and the detected results can be presented to the drivers in a video way, while the detection distance is less than the infrared and radar, but it is still popular.
  - Lane Departure Warning System

    The main function of LDWS operation is to produce lane detection results through image processing and calculation. Once the vehicle starts to drift in the absence of lane direction light situation, the system will automatically sends warning signal, to remind the driver to react immediately to avoid accidents.
  - Forward Collision Warning System

    The  main function of FCWS is to detect and estimate the distance from the car in front. When the two cars don't maintain a safe distance, the system will automatically send a warning to the driver. Even in recent years, the car factories have estimated the function to replace the brake with computer system.

Considering the main function of two Advanced Driver Assistance System, the common point is that they may use the same image information from camera, and the camera can also store the image information. So the manufactures of driver recorder also are actively involved in research and development.

Now digital image processing platform can be divided into two
categories, software-oriented PC-based platform and hardware oriented stand alone platform. And the two each has its advantages and disadvantages.

Early, because the system of independent computing resources is very limited, the CPU instruction cycle is slow, and insufficient memory and the shortage of effective image programming interface lead that image processing algorithm developers used PC-based as hardware platform. Now, because of Very Large Scale Lntegrated circuit (VLSI) and System on Chip (SoC) development, digital system chip has substantial progress in narrowing of the progress at the same time. The embedded system can be in a low price situation, including the surrounding control of high speed CPU, mass storage. And even more, there can be multiple core processors.

For the ADAS algorithm performance must remain, because when the dangerous condition occurs, the system must be real-time to issue a warning. In the case of FCWS, when a vehicle is on the highway with 100 kilometers per hour, which is 27.7 meters forward per second. If the system's delay is 0.1 second, the vehicle will continue to move forward 2.77 meters, so the designers used to use Frame Per Second to verify the effectiveness of the algorithm in ADAS. If the FPS value is larger, the algorithm performance is better.

Of course, the performance of algorithm is related to the speed and resources of embedded processors. So in the early stage of development, we must consider the embedded platform which the computation of the algorithm is consistent with, otherwise the dilemma, the algorithm has not appropriate platform, will occure.

# Objectives
With the fast image segmentation results ,lane detection algorithm has robust performance.

lane departure warning system, the early-developed function, mainly includes lane detection and lane offset detection. Although the research has more than ten years of history, the research results need more improvement. 

In the lane boundary detection, because of the disability to predict lane and the gap between the color degree,gradient, we cannot use the default parameter in the algorithm to detect all types of lane. In addition, we often need to overlap multiple consecutive frames, to lengthen the length of the lane. Finally, the traditional lane system, need many pictures to mark lane possible area to filter out non lane route because of the linear detection algorithm.

In the lane departure detection, we need the analysis of continuous pictures, so the system must be real time.
So we want to use one picture for detection to achieve real-time analysis.

# Methods
By combining the fast image segmentation results, lane detection algorithm can detect the lane lines of various types, without the analysis of continuous pictures. It can only need a picture to judge whether the vehicle is offset.
## Lane Detection Algorithm
1. Image Segmentation

    First, regionize the original image, and divide the image into several areas.
1. Road Line Region Detection

    Then combine the image segmentation results after gradient analysis, to complete the lane region detection.

1. Road Candidate Determination

    Use Connected Component Labeling to mark each connected component, and then analyzes the characteristics of each region.
1. Road Line Determination

    Then detect the lines of lanes.
1. Lane Departure Warning

    Finally, judge the angle of left and right lane, in order to complete the function of warning.

Of course, we can combine it with some research in the algorithm of  vehicles in front based on the horizontal line.




