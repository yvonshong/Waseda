package lesson6;

/**
 * Created by yvon- on 12/8/2016.
 */
public class ProductA1 extends ProductA {
    public ProductA1(int index,int numOfOpe){
        setIndex(index);
        setNumOfOpe(numOfOpe);
    }
    protected  int getCost(){
        return getpIndex()*getNumOfOpe()*getNumOfOpe();
    }
}
