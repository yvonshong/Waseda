package lesson1;
public class Factorial {

    public static long factorial(long number) {
        if (number <= 1)
            return 1;
        else
            return number * factorial(number - 1);
    }
    
    public static long tailFactorial(long number,long lastResult) {
        if (number <= 1)
            return lastResult;
        else
            return tailFactorial(number-1,number*lastResult);
    }
    

    public static void main(String args[]) {
    	System.out.println("factorial:");
        int n = 10;
        System.out.println(factorial(n));
        //尾递归 传参减少堆栈优化
        System.out.println("tail factorial:");
        System.out.println(tailFactorial(n,1));
        
        System.out.println("while:");
        int counter=n;
        int result1=1;
        while(counter>0)
        {
        	result1=result1*counter;
        	counter--;
        }
        System.out.println(result1);
        
        System.out.println("for:");
        int result2=1;
        for(int i=1;i<=n;i++)
        {
        	result2=result2*i;
        }
        System.out.println(result2);
        
        
    }
}