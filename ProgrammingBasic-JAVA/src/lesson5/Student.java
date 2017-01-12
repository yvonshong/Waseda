package lesson5;

/**
 * Created by yvon- on 12/1/2016.
 */
public abstract class Student {
    protected String nationality;
    protected String course;
    protected double point;
    protected double score;

    protected abstract double getScore();

    public void output(){
        System.out.println("nationality: "+nationality+" course: "+course+" point: "+point+" score: "+getScore());
    }


}
