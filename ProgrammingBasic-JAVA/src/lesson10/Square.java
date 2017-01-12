package lesson10;

public class Square extends Rectangle {
	double edgeLength;
	String getType(){return "Square";}
	Square(double w){
		edgeLength = w;
	}
	double getArea(){
		return edgeLength * edgeLength;
	}
}
