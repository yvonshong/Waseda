factorial

```java
public class Factorial {
    public static long factorial(long number) {
        if (number <= 1)
            return 1;
        else
            return number * factorial(number - 1);
    }
    
    public static long tailFactorial(long number,long lastResult) {
        if (number <= 1)
            return lastResult;
        else
            return tailFactorial(number-1,number*lastResult);
    }
    
    public static void main(String args[]) {
    	System.out.println("factorial:");
        int n = 10;
        System.out.println(factorial(n));
        //尾递归 传参减少堆栈优化
        System.out.println("tail factorial:");
        System.out.println(tailFactorial(n,1));
        
        System.out.println("while:");
        int counter=n;
        int result1=1;
        while(counter>0)
        {
        	result1=result1*counter;
        	counter--;
        }
        System.out.println(result1);
        
        System.out.println("for:");
        int result2=1;
        for(int i=1;i<=n;i++)
        {
        	result2=result2*i;
        }
        System.out.println(result2);   
    }
}
```

- 类的成员和方法

- set方法和get方法

- 子类的重载构造函数和重载方法

- 子类重载父类的抽象类（抽象类和接口的区别）

- 子类重载抽象类（子类重载抽象类和构造函数）

- 文件流处理

```java
import java.io.*;
public class Demonstrate {
    public static void main(String argv[]) throws IOException {
        FileInputStream stream = new FileInputStream("C:\\Users\\yvon-\\IdeaProjects\\Demonstrate\\src\\lesson7\\20161215E1.txt");
        InputStreamReader reader = new InputStreamReader(stream);
        BufferedReader buffer = new BufferedReader(reader);
        String line;
        while ((line = buffer.readLine()) != null && !line.equals("")) {
            line= line.trim();
            String list[];
            list=line.split(" ");
            String tempSex = list[0];
            double tempHeight=Double.parseDouble(list[1]);
            double tempWeight = Double.parseDouble(list[2]);
            Person temp = new Person(tempSex,tempHeight,tempWeight);
            temp.output();
        }
        stream.close();
        return;
    }
}

```

```java
while ((line = buffer.readLine()) != null && !line.equals("")) {
    line= line.replace('\t',' ').trim();
    int nextSpace = line.indexOf(" ");
    int w = Integer.parseInt(line.substring(0, nextSpace));
    int h = Integer.parseInt(line.substring(nextSpace).trim());
    Rectangle rect = new Rectangle();
    rect.width = w;
    rect.height = h;
    System.out.println("area = " + rect.getArea());
}
```



```java
public static String strInsert(String a,String b,int t){
        return a.substring(0,t)+b+a.substring(t,a.length());
    }
```

数组

```java
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
```