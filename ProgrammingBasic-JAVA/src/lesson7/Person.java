package lesson7;

/**
 * Created by yvon- on 12/15/2016.
 */
public class Person {
    public String sex;
    public double height;
    public double weight;
    public Person(String sex,double height,double weight){

        this.sex=sex;
        this.height=height;
        this.weight=weight;
    }
    void output(){
        System.out.println("Sex: "+sex+" height: "+height+" weight: "+weight);
    }
}
