package lesson10;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.ListIterator;

public class Demonstrate {
   public static void main (String argv[])  throws IOException {
      FileInputStream stream = new FileInputStream("C:\\Users\\yvon-\\IdeaProjects\\Demonstrate\\src\\lesson10\\20161222E.txt");
      InputStreamReader reader = new InputStreamReader(stream);
      BufferedReader buffer = new BufferedReader(reader);
      String line;


      ArrayList<Figure> figList = new ArrayList<Figure>();

      while ((line = buffer.readLine()) != null && !line.equals("")) {
         String sentence = line.trim();

         String strArray[]=sentence.split(" ");
         if(strArray[1].equals("Rectangle"))
         {
            figList.add(new Rectangle(Double.parseDouble(strArray[2]),(Double.parseDouble(strArray[3]))));
         }

         if(strArray[1].equals("Circle")){
            figList.add(new Circle(Double.parseDouble(strArray[2])));
         }
         if(strArray[1].equals("Square")){
            figList.add(new Square(Double.parseDouble(strArray[2])));
         }

      }
      stream.close();
      int i = 1;
      ListIterator<Figure> figIt = figList.listIterator();
      while(figIt.hasNext()){
         Figure fig = figIt.next();
         System.out.println(i+"   "+fig.getType()+"   "+fig.getArea());

         i++;


      } 
      

  } 
} 

