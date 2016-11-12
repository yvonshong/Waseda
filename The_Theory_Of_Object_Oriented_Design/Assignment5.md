# OOD Assignment 5
Authored by: **SONG DAIWEI**  44161588-3

# Describe the following items with the reason why these technologies are needed.

## AOP: Aspect Oriented Programming: What issues are tackled by AOP?

Typically, an aspect is scattered or tangled as code, making it harder to understand and maintain. It is scattered by virtue of the function (such as logging) being spread over a number of unrelated functions that might use its function, possibly in entirely unrelated systems, different source languages, etc. That means to change logging can require modifying all affected modules. Aspects become tangled not only with the mainline function of the systems in which they are expressed but also with each other. That means changing one concern entails understanding all the tangled concerns or having some means by which the effect of changes can be inferred.
[ \(1\) ][1]




Aspect-Oriented Programming (AOP) is a programming paradigm that aims to increase modularity by allowing the separation of cross-cutting concerns. AOP forms a basis for aspect-oriented software development. AOP includes programming methods and tools that support the modularization of concerns at the level of the source code.

It aimed at dividing the log records, performance statistics, security control, transaction processing, exception handling code from the business logic code. Through the separation of these behaviors, we hope they can be independent to the non business logic method, and it won't affect the business logic code when we change these behaviors.


## POJO: Plain Old Java Object: What problems does POJO try to solve?


Firstly, upgrading to new, better version of infrastructure framework is difficult and impossible because of Enterprise Java
    and incompatible standards of EJB 1, EJB 2 and EJB, many persistence options like EJB CMP 1/2, Hibernate 1/2/3, JDO 1/2 and EJB 3 persistence.

And it makes development more difficult, which forces developers to think about business logic and infrastructure concerns simultaneously.

And it makes testing more difficult, because we must deploy code/tests in application server, and slow down the edit-compile-debug cycle.

Whatsmore, EJB 2 prevented OO development and its application servers are complex and some are expensive.
[ \(2\) ][2]



POJO are Java objects that don't implement
any special interfaces or (perhaps)
call infrastructure APIs.

Decoupling the application code from the infrastructure frameworks is one of the many benefits of using POJOs. Using POJOs future proofs your application's business logic by decoupling it from volatile, constantly evolving infrastructure frameworks. Upgrading to a new version or switching to a different framework becomes easier and less risky. POJOs also make testing easier, which simplifies and accelerates development. Your business logic will be clearer and simpler because it won't be tangled with the infrastructure code.

## DI: Dependency Injection: What issues does DI try to solve?

Because most applications are composed of two or more classes through cooperation with each other to implement the business logic, which makes each object needs to gain the refenrence of cooperative object (the object it depends on). If this acquisition process depends on its own implementation, it will lead to a high degree of coupling and it is difficult to maintain and debug.


So in software engineering, dependency injection is a software design pattern that implements inversion of control for resolving dependencies. A dependency is an object that can be used (a service). An injection is the passing of a dependency to a dependent object (a client) that would use it. The service is made part of the client's state.[1] Passing the service to the client, rather than allowing a client to build or find the service, is the fundamental requirement of the pattern.

This fundamental requirement means that using values (services) produced within the class from new or static methods is prohibited. The class should accept values passed in from outside.[ \(3\) ][3]


# Refenrence
[1]:https://en.wikipedia.org/wiki/Aspect-oriented_programming
[1] https://en.wikipedia.org/wiki/Aspect-oriented_programming


[2]:http://www.chrisrichardson.net/presentations/OverviewOfPOJOs_BEA_JUG.pdf
[2] http://www.chrisrichardson.net/presentations/OverviewOfPOJOs_BEA_JUG.pdf


[3]:https://en.wikipedia.org/wiki/Dependency_injection

[3] https://en.wikipedia.org/wiki/Dependency_injection
