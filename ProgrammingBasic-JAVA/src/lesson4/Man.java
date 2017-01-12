package lesson4;

/**
 * Created by yvon- on 11/17/2016.
 */
public class Man extends Human{
    public Man(double height,double weight){
        setHeight(height);
        setWeight(weight);
        setBMI(height,weight);
        this.gender="male";
    }


    public void popMessage(){
        if(BMI>30)
            message = "CAUTION!";
        else
            message = "Keep fit!";
        System.out.println(message);
    }


}
