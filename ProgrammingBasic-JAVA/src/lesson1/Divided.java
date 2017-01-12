package lesson1;

public class Divided {
	public static void main(String args[]) {
		int n=11;
		for(int i=99;i>0;i--)
			if(i % n == 0)
			 {
				System.out.println("the max integer between 1 - 99 can be divided by "+n+" is ");
				System.out.println(i);
				break;				
			 }				
	}
}
