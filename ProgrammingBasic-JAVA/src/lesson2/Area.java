package lesson2;

public class Area {
    public static void main (String argv[]) { 
        Rectangle rect = new Rectangle();
        rect.width = 10; 
        rect.height = 6;
        System.out.println("width =  " + rect.width); 
        System.out.println("height =  " + rect.height); 
        
        Square sqr=new Square();
        sqr.width=compare(rect.width, rect.height);
        System.out.println("width of the square which is the longer side of rectangle =  " + sqr.width); 

        System.out.println("the larger area =  " + compare(sqr.area(),rect.area())); 
        
        
    } 
    public static int compare(int a,int b)
    {
    	if (a>b)
    		return a;
    	else
    		return b;
    }
    
    
}
