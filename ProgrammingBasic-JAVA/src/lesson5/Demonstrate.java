package lesson5;

/**
 * Created by yvon- on 12/1/2016.
 */
public class Demonstrate {
    public static void main (String argv[]) {
        nationalStudent nS = new nationalStudent("JanpaneseCourse",1,100);
        internationalStudent iS =new internationalStudent("internationalCourse",2,100);
        nS.output();
        iS.output();
    }
}
