package lesson6;

/**
 * Created by yvon- on 12/8/2016.
 */
public class Main {
    public static void main (String argv[]) {
        ProductA pA = new ProductA();
        pA.setIndex(2);
        pA.setNumOfOpe(10);
        ProductA1 pA1=new ProductA1(3,100);
        pA.output();
        pA1.output();
    }
}
