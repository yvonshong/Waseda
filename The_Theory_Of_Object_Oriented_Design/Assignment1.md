# What issues did “Structured Programming” try to solve? And what issues remained unsolved?
In traditional programing, many people use transfer statement which will leads to big chaos. And there are many very free flow line to describe the transfer function in the traditional flow chart. It is hard to understand and accept. 

So in the development of software, we use structured programing languages including conditional statements and loops.

However, it becomes unwieldy as your application grows to great size. For example, the data and operation are separated from each other, which leads to if there is data structure changes, the corresponding operation function will have to rewrite. If there is demand changes or new requirements, it may also lead to re-division of modules. In process oriented programming, data and operations are separated from each other, so that some modules are combined with the specific application environment, and the old program modules are difficult to be reused in the new program. The disadvantages of these process oriented programming ideas make it more and more difficult to adapt to the development of large software projects.

# How does “OOD” try to solve those unsolved issues?
Object Oriented Programming is designed which focuses on data, and it follows bottom-up approach.

Object Oriented Programming supports inheritance, encapsulation, abstraction,polymorphism, etc, to regard everything as an object. And it is easy to design the produce and implement, because we can reuse the design and code, which improves the efficiency of programing. In Object Oriented Programming, Programs are divided into small entities called objects,which can solve any complex programs.

Object Oriented Programming provides more reusability, less function dependency, more abstraction and more flexibility, and it is more secure as having data hiding feature.

# And, what issues remains now?

In object-oriented programming, most developers are accustomed to programming in the imperative/procedural style. To switch to developing in a pure functional style, they have to make a transition in their thinking and their approach to development. To solve problems, OOP developers design class hierarchies, focus on proper encapsulation, and think in terms of class contracts. The behavior and state of object types are paramount, and language features, such as classes, interfaces, inheritance, and polymorphism, are provided to address these concerns. And it will lead to increased workload. If you blindly emphasize encapsulation, when the object is modified, the object of any property is not allowed to access the external direct, which means that you should increase methods only responsible for reading or writing behavior. This will increase the burden on the programming work, increase the running cost, and make the program appear bloated.

Also there is low performance problem. Due to the higher level of the logical abstraction layer, the object oriented programing had to sacrifice the performance, and the calculation of time and storage space are very large.

But some things are not objects. In OOP, we focus on the data of the operation, so we can add different type of data in one operation. But how about another method? How could I add another function to the data? Maybe we will rewrite a lot again. Function is not an object. 

# What technologies are proposed in order to solve those issues now?
Function programing.
Functional programming is a paradigm which concentrates on computing results rather than on performing actions. That is, when you call a function, the only significant effect that the function has is usually to compute a value and return it. Of course, behind the scenes the function is using CPU time, allocating and writing memory, but from the programmer's point of view, the primary effect is the return value. Objects in a functional programming language are often immutable (a.k.a. const or final); instead of changing an object, you allocate a new object which looks like the old one except for the change. Compare this with an imperative programming language like Java, where progress is made by changing objects' fields, inserting them into sets, etc.