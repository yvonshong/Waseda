package lesson4;

/**
 * Created by yvon- on 11/17/2016.
 */
public class Human {
    public double height,weight;
    public double BMI;
    public String gender,message;



    public void setBMI(double height, double weight){
        this.BMI=weight/(height*height);
    }

    public double getBMI(){
        return BMI;
    }
    public String getGender(){
        return gender;
    }

    public void setHeight(double height){
        this.height=height;
    }
    public void setWeight(double weight){
        this.weight=weight;
    }

    public void popMessage(){
    }

    public void output(){
        System.out.println("Gender: "+gender+" BMI: "+BMI);
        popMessage();
    }


}
