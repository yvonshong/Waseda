package lesson10;

/**
 * Created by yvon- on 1/12/2017.
 */
public class Circle  extends Rectangle  {
    double radius;
    String getType(){return "Circle";}
    double Pi=3.14;
    Circle(double r){ radius = r;}

    double getArea(){
        return Pi*radius*radius;
    }

}
