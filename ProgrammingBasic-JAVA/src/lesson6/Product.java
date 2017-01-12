package lesson6;

/**
 * Created by yvon- on 12/8/2016.
 */
public abstract class Product {
    private int pIndex;
    private int numOfOpe;

    protected void setIndex(int index){
        this.pIndex = index;
    }

    protected void setNumOfOpe(int NumOfOpe){
        this.numOfOpe=NumOfOpe;

    }

    protected int getpIndex(){
        return this.pIndex;
    }
    protected int getNumOfOpe(){
        return this.numOfOpe;
    }
    protected abstract int getCost();

    protected void output(){
        System.out.println("Index: "+getpIndex()+" numOfOpe: "+getNumOfOpe()+" cost: "+getCost());

    }


}
