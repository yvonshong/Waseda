package lesson2;

public class StudentCaller {
    public static void main (String argv[]) { 
        Student song = new Student();
        song.name="SONG DAIWEI";
        song.number=44161588;
        song.age=21;
        song.gender="male";
        song.nationality ="China";
        System.out.println("The nationality of "+song.name+ " is "+ song.nationality); 
        System.out.println("The gender of "+song.name+ " is "+ song.gender); 
        System.out.println("The age of "+song.name+ " is "+ song.age); 
        System.out.println("The number of "+song.name+ " is "+ song.number); 
    } 
}
