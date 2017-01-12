package lesson5;

/**
 * Created by yvon- on 12/1/2016.
 */
public class nationalStudent extends Student {
    public nationalStudent(String course,double point,double score){
        this.nationality="Japan";
        this.course=course;
        this.point=point;
        this.score=score;
    }
    protected double getScore(){
        return this.score*this.point;
    }
}