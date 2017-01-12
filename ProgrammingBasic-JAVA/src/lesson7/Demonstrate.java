package lesson7;
import java.io.*;
/**
 * Created by yvon- on 12/15/2016.
 */


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

