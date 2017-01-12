package lesson10;

public class Rectangle extends Figure {
	double width, height;

	String getType(){return "Rectangle";}
	Rectangle(){}
	Rectangle(double w, double h){
		width = w;
		height = h;

	}
	double getArea(){
		return width * height;
	}
}
