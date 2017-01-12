package lesson9;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Created by yvon- on 12/22/2016.
 */
public class Main {


    public static void main(String argv[]) throws IOException{
        FileInputStream stream = new FileInputStream("C:\\Users\\yvon-\\IdeaProjects\\Demonstrate\\src\\lesson9\\20161222E.txt");
        InputStreamReader reader = new InputStreamReader(stream);
        BufferedReader buffer = new BufferedReader(reader);
        String line;
        int counter=0;
        while ((line = buffer.readLine()) != null && !line.equals("")) {
            if(counter==0) {
                line = line.replace(" ", "");
                int integer=Integer.parseInt(line);
                System.out.println("integer: "+integer);
                String str = String.valueOf(integer);
                System.out.println("String format output:");
                for(int i=0;i<str.length();i++)
                {
                    System.out.println(str.charAt(i));
                }
            }
            if(counter==1){
                String sentence = line.trim();
                //Addspacestome.
                sentence = strInsert(sentence," ",3);
                //Add spacestome.
                sentence = strInsert(sentence," ",10);
                //Add spaces tome.
                sentence = strInsert(sentence," ",13);
                //Add spaces to me.
                System.out.println(sentence);
                String strArray[]=sentence.split(" ");
                for(int j= strArray.length;j>0;j--){
                    System.out.println(strArray[j-1]);
                }
            }
            counter++;
        }
        stream.close();
    }
    public static String strInsert(String a,String b,int t){
        return a.substring(0,t)+b+a.substring(t,a.length());
    }
}