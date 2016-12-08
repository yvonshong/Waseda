fib := method(n,if(n > 2, fib(n-1) + fib(n-2), 1));
n := 0
while(fib(n) < 30000, fib(n) println; n = n + 1)