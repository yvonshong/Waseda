package lesson5;

/**
 * Created by yvon- on 12/1/2016.
 */
public class internationalStudent extends Student {
    public internationalStudent(String course,double point,double score){
        this.course=course;
        this.point=point;
        this.score=score;
        this.nationality="Foreign";
    }
    protected double getScore(){
        return this.score*this.point*1.2;
    }
}
