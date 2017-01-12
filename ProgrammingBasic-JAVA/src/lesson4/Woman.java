package lesson4;

/**
 * Created by yvon- on 11/17/2016.
 */
public class Woman extends Human{
    public Woman(double height,double weight){
        setHeight(height);
        setWeight(weight);
        setBMI(height,weight);
        this.gender="female";
    }
    public void popMessage(){
        if(BMI>25)
            message = "CAUTION!";
        else
            message = "Keep fit!";
        System.out.println(message);
    }
}
