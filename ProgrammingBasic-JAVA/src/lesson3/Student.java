package lesson3;

/**
 * Created by yvon- on 11/17/2016.
 */

public class Student{
    public String name;
    public int id;

    public Student(String name,int id)
    {
        setName(name);
        setId(id);
    }


    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public int getId() {
        return id;
    }
    public void setId(int id) {
        this.id = id;
    }

    public void output(){
        System.out.println("Name: "+name+" and ID: "+id);
    }


}
