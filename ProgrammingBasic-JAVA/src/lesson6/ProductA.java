package lesson6;

/**
 * Created by yvon- on 12/8/2016.
 */
public class ProductA extends Product {
    protected  int getCost(){
        return getpIndex()*getNumOfOpe();
    }
}
