package lesson8;

public class Rectangle {
    int height, width;

    protected int getArea(){
        return calculateArea();
    }
    protected int calculateArea(){
        return height*width;
    }
}
