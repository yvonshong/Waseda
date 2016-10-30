# What is “global data"? And why is it needed in a structured programming?

Global data is a piece of data with global scope, meaning that it is visible throughout the program, unless shadowed. The set of all global data is known as the global environment or global state.

In compiled languages, global variables are generally static variables, whose extent (lifetime) is the entire runtime of the program.

Though in interpreted languages (including command-line interpreters), global variables are generally dynamically allocated when declared, since they are not known ahead of time. (1)

I think that the differences between global data and local variable are list as follows.

- In the scope of effect, it is visible from everywhere in the program.

- In the life cycle, it runs through the program.

And because of the two points list, the access across the unit and its life-cycle, it is dangerous.


Of course, there are some advantages for it, so it is needed in a structured programming. Global variables can be used to avoid having to pass frequently-used variables continuously throughout several functions. In practice, large programs may well require a large number of global variables because there are so many parameters that are shared between different functions, and care must be taken to make sure different functions share the global data without mishap. (1)

In summary, it can reduce the total number of variables, and the cost of passing the actual and formal arguments.

And global variables are used extensively to pass information between sections of code that do not share a caller / callee relation like concurrent threads and signal handlers. (2)

# Show how to treat the global data in OOP and describe the reason why the global data in OOP is treated like that.

## Advantages of it in OOP:

In OOP, inheritance and polymorphism are designed for the variety of functions and methods.

The emphasis of data in OOP is encapsulation.

In OOP, the data can be packaged as class members of objects.

And as class members, it could have more detailed authority and it is good to debug. I mean, anything without the access to class member cannot modify the data, which is much safer.



## Disadvantages of it in structure programming:

There are some basically disadvantages. Global data runs through the program and takes up more memory.

- It destroys the encapsulation of the function.

    The statements in the body of the function can bypass the function parameters and return values to input or output. This situation undermines the independence of the function, so that the function depends on the global data. At the same time, it also reduces the portability of the function. (3)

- It is unsafe.

    Global data can potentially be modified from anywhere (unless they reside in protected memory or are otherwise rendered read-only), and any part of the program may depend on it.

- It leads to the complexity of the program.

    Global data therefore has an unlimited potential for creating mutual dependencies, and adding mutual dependencies increases complexity.


Global data also make it difficult to integrate modules because software written by others may use the same global names unless names are reserved by agreement, or by naming convention. Besides, without proper locking, code using global variables will not be thread-safe except for read only values in protected memory. (1)

# Reference:

1. Global variable, Wikipedia, https://en.wikipedia.org/wiki/Global_variable

2. Lee213, "The advantages and disadvantages of global variable." 2011.06.13 http://blog.chinaunix.net/uid-8947853-id-370962.html

3. Sen Zhangai, "the summary of the encapsulation in object oriented programming. " 2016.06.07 https://senzhangai.github.io/programming/about-oop-encapsulation#%E6%95%B0%E6%8D%AE%E5%B0%81%E8%A3%85%E4%B8%8E%E5%85%A8%E5%B1%80%E5%8F%98%E9%87%8F