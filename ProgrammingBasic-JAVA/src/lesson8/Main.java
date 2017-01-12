package lesson8;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

public class Main{


    public static void main(String argv[]) throws IOException{
        FileInputStream stream = new FileInputStream("C:\\Users\\yvon-\\IdeaProjects\\Demonstrate\\src\\lesson8\\20161215E2.txt");
        InputStreamReader reader = new InputStreamReader(stream);
        BufferedReader buffer = new BufferedReader(reader);
        String line;
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
        stream.close();
    }
}