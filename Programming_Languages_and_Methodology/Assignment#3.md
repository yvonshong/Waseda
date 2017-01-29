# Programming Language and Methodology Assignment#2 
SONG DAIWEI #44161588-3

We have already learnt about the following programming languages
Ruby/Io/Prolog/Scala/Clojure/Haskell

# Classify the above 6 programming languages, and discuss the advantages and disadvantages for each programming language. (A4paper at least 2 pages)



## Ruby

Yukihiro Matsumoto invented about Ruby in 1993, we call him Matz. From a linguistic point of view, Ruby is derived from the so called scripting language family, which is interpreted, object-oriented, and dynamic. Interpretation means that the Ruby code is executed by the interpreter rather than the compiler. Dynamic type, which means that the type is bound to be compiled at run time. From these two aspects, the Ruby strategy is to find a balance between flexibility and run-time security, we will discuss this later. Object oriented, which means that Ruby supports encapsulation (which encapsulates data and behavior), class inheritance (using a tree to organize object types), polymorphism (objects can be expressed in a variety of forms) and so on. Ruby has been quietly dormant, waiting for a proper timing. Finally, with the framework of Rails Ruby emerged in 2006. After ten years in the jungle of enterprise development, Ruby has brought people back to the fun of programming. Although from the execution speed aspect, Ruby is not much more efficient, but it can make programmers programming efficiency greatly improved.



### Benefits of Ruby on Rails

So why Ruby on Rails? We find Ruby provides us with a combination of the best tooling, better quality code libraries and a more pragmatic approach to software. Plus, the Ruby community tends to have a higher calibre of engineer, who favour responsible development over the gung-ho approach that can be seen in other communities.

- Tooling - Rails provides fantastic tooling that helps you to deliver more features in less time. It provides a standard structure for web apps, where all the common patterns are taken care of for you.
Libraries - There's a gem (3rd party module) for just about anything you can think of. They are all publicly available and searchable through https://rubygems.org/.
- Code Quality - Generally, we find the quality of third party Ruby code to be significantly higher than their PHP or NodeJS equivalents.
Test Automation - The Ruby community is big in to testing and test automation. We believe this is incredibly valuable in helping to deliver good quality software and is one of the reason the Ruby libraries are so great.
- Large Community - Pretty much every major city in the world has a Ruby community that runs regular meetups. It's one of the most popular languages on social coding site Github.
- Popular in The Valley - History has shown that technology that's been popular within Silicon Valley has gradually been adopted across the world. If you look at the big startup successes of recent years, such as Airbnb, Etsy, GitHub & Shopify - they are are all on Ruby on Rails.
- Responsible Developers - You tend to find Ruby developers are more closely aligned around the the rules of responsible development. If you start small, communicate well, tackle vertical slices, write simple code over smart code, share ownership etc, you tend to find your project ends up in better shape.
- Productivity - Ruby is an eloquent and succinct language, which when combined with the plethora of 3rd party libraries, enables you to development features incredibly fast. I would say it's the most productive programming language around.
- Next Generation - Ruby on Rails seems to be the language of choice for a number of the popular online code schools, such as Makers Academy, Steer and CodeCademy. This should mean an increase in talented programmers joining the Ruby community over the coming years.

### Disadvantages of Ruby on Rails?

Of course Rails does have have its disadvantages and it's only fair that we share those in this post.

- Runtime Speed - The most cited argument against Ruby on Rails is that it's "slow". We would agree, certainly when compared to the runtime speed of NodeJS or GoLang. Though in reality, the performance of a Ruby application is incredibly unlikely to be a bottleneck for a business. In 99% of cases, the bottleneck is going to be elsewhere, such as within the engineering team, IO, database or server architecture etc. When you get to a significant enough scale to have to worry about Rails runtime speed, then you're likely to have a incredibly successful application (think Twitter volume) and will have many scaling issues to deal with.
- Boot Speed - The main frustration we hear from developers working in Rails is the boot speed of the Rails framework. Depending on the number of gem dependencies and files, it can take a significant amount of time to start, which can hinder developer performance. In recent versions of Rails this has been somewhat combatted by the introduction of Spring, but we feel this could still be faster.
- Documentation - It can be hard to find good documentation. Particularly for the less popular gems and for libraries which make heavy use of mixins (which is most of Rails). You'll often end up finding the test suite acts as documentation and you'll rely on this to understand behaviour. This isn't itself a bad thing, as the test suite should be the most up-to-date representation of the system, however, it can still be frustrating having to dive into code, when sometimes written documentation would have been much quicker.
- Multithreading - Rails supports multithreading, though some of the IO libraries do not, as they keep hold of the GIL (Global Interpreter Lock). This means if you're not careful, requests will get queued up behind the active request and can introduce performance issues. In practice, this isn't too much of a problem as, if you use a library that relies on GLI, you can switch to multiprocess setup. The knock-on effect of this is your application ends up consuming more compute resources than necessary, which can increase your infrastructure costs.
- ActiveRecord - AR is used heavily within the Ruby on Rails world and is a hard dependency for many of the RubyGems. Although we think it's a great design pattern, the biggest drawback we see is that your domain becomes tightly coupled to your persistence mechanism. This is far from ideal and can lead to bad architecture decisions. There are many ways to work around this, some of which are included in this 7 Patterns to Refactor Fat ActiveRecord Models article. We would like to see Rails become less reliant on ActiveRecord.



## Io

The language was created by Steve Dekorte in 2002, after trying to help a friend, Dru Nelson, with his language, Cel. He found out that he really didn't know much about how languages worked, and set out to write a tiny language to understand the problems better.

### Advantages:

Very consistent syntax; Very minimalistic rules; Has a lot of useful methods that makes your life easier; Very polished source code; Very informative and polished errors; Chic documentation.
And it has pure object-oriented based on prototypes; code-as-data / homoiconic; lazy evaluation of function parameters; higher-order functions; introspection, reflection and metaprogramming; Actor-based concurrency; Coroutines; exception handling; incremental garbage collecting supporting weak links; highly portable; DLL/shared library dynamic loading on most platforms; small virtual machine

### Disadvantages:
- Indexing begins from 0 not from 1.
- You get the byte representation and not the letter itself.
- Resulting elseif structures look a bit ugly.
- No GUI library bindings yet
- Of course these are all subjective.
- It does not have GUI binding yet. But once it has, then it will be a killer programming language.


##  Prolog

Prolog and the first two chapters of the programming language is different. Io and Ruby are called imperative languages (imperative language). Command language is like a recipe for cooking, you need to tell the computer exactly how to do a job. A higher level of command language may give you more leverage, and more than one long step will be merged into one step. But basically, you're actually making a list of the ingredients and describing the details of the baked cake.


### Advantages	
Logic based languages are able to represent the real world more accurately.
Prolog is able to derive new rules from the existing rules contained within the knowledge base.

### Disadvantages

It can be very difficult to design a database that accurately represents relationships.
Prolog is not best suited to solving complex arithmetical computations.
Prolog programs are not best suited to the current PC architecture (sequential execution) and are best optimised on parallel architectures (fifth generation computers).

## Scala

The close relationship between Scala and Java
Scala can be used as a bridge at least, not only that. It is closely integrated with Java, providing people with an opportunity to protect investment, which is reflected in the following areas.
Scala runs on the Java virtual machine, which allows Scala to run with existing applications.
Scala can be directly used by the Java class library, allowing developers to leverage existing frameworks and legacy code.
Scala is the same as Java, so the two languages follow the same programming philosophy.
Scala syntax and Java closer, allowing developers to quickly grasp the language foundation.
Scala not only supports the object-oriented paradigm also supports functional programming paradigm, so that developers can be gradually applied to functional programming in the code of thought.

### Advantages
Simple and straightforward syntax. Scala typically requires two-thirds less code than Java. The syntax is also more flexible. For example, you can leave out periods between method calls so the code is more human-readable and easier to understand.
Inherently immutable objects. Scala’s programming language reduces many thread-safety concerns that spring up in traditional Java applications.
Highly functional. Scala treats functions as first-class citizens.
Fast implementation speed. It allows for quicker implementation and enhanced performance.
It is fun! Scala challenges strong engineers in a meaningful and entertaining manner, making development more fun.
Easy to solve concurrency issues. It has an Actor library to solve concurrency problems more rapidly.
XML support. Scala supports XML, which is beneficial if you have a need to encode documents in your products.

### Disadvantages
Limited community presence. Since the language is new, there are limited online communities and resources to help you troubleshoot issues. Unlike the Grails or Rails communities, Scala is still gaining followers and experts. If your developers do not have the time to resolve issues themselves, Scala may not be a viable option for you.
Hard to learn. Syntactically, Scala is significantly different from traditional Java. Furthermore, it presents a completely different programming paradigm—requiring a higher level, more sophisticated developer/engineer skill set to understand it. As a result, Scala developers are in low supply and costly.
Lack of ease of adoption. When coding with a team, it’s rare to find everyone writing purely in Scala. You need a team of eager adopters for Scala to be feasible. Otherwise, you will end up with code that’s a hybrid of Scala and Java, which can cause problems. Ramp up time can be very slow and time consuming. And it’s very difficult to find/build a team of pure Scala developers.
Limited backward compatibility. Each major new release of Scala is incompatible with the previous version. This leads to a lot of wheel-reinventing, headaches and product delays. If your development schedule is tight, you should avoid using Scala, for now, to stay on track.
 
## Erlang
Like Erlang so full of mysterious language. This concurrent language can be difficult, but also easy to become difficult. In the case of robust enterprise deployment, its virtual machine BEAM is the only rival worthy of the Java virtual machine. It is very efficient, even outside of efficiency, which is seldom considered. Therefore, its syntax is not as elegant and concise as Ruby.

### advantages
Lightweight user-space threads.
Built-in distribution and failure detection.
Mostly functional programming with decent matching constructs, but with fully dynamic typing.
Reliability-oriented standard library.

### disadvantages
Error reporting is terrible.  This might just be my Python experience forcing me to think the wrong way about exceptions, but I find debugging exceptions in Erlang to be unnecessarily difficult.  Stack traces don't have line numbers, which makes it impossible to determine which makes it impossible to determine which match caused a "badmatch" error (though the smart_exceptions module at http://jungerl.sourceforge.net/ helps).  No exceptions include human-readable descriptions of the problem, and few contain specific information about the cause of the error (peer address for socket errors, for example).  Errors that kill multiple processes via links are even more difficult to read.

The debugger is not good enough.  Erlang comes with a very powerful debugger for sequential programs (though it is lacking in ease-of-setup), but it doesn't integrate with the standard library well enough to make it a good debugger for parallel programs (e.g., "step into" doesn't work for gen_server:call).  Erlang also provides runtime hooks that could be used to build solid debugging tools for parallel programs, but they are too low-level for most users.  I would love to see some "debugging middleware" that makes it much easier to do some ad-hoc analysis of the state of an Erlang program.

Single variable assignment is often annoying.  Immutable data has nice properties, but that feature is completely orthogonal to only allowing a single assignment to a variable, which often results in variables called "X1", "X2", "X3", etc.  See "Functional Programming Mismatch" at http://damienkatz.net/2008/03/wh... for more details.

Vim's indent mode for Erlang is not good enough.  I have to use Emacs to edit Erlang code, which results in a drop in my productivity, even with viper and vimpulse.
## Clojure
Clojure is a Lisp implementation on JVM. Lisp is complex and powerful, is one of the earliest and most recent programming languages in the computer field. Many have tried to squeeze into the mainstream Lisp dialect language ranks, but failed. Even for today's developers, the syntax and programming model is a bit hard to digest. Even so, Lisp's character is still so that people can't help to go back, to finish, new best in some dialects emerge in an endless stream, programming field to help students create innovative, open way of thinking with Lisp language.
### Advantages
it's beautiful and clean. It addresses in a natural way issues of parallelization and scale. And if you have some basic java or OOP knowledge, you can benefit from the large number of high-quality java libraries.


### Disadvantages
- very risky
- very immature
- with niche adoption. 

Great for enthusiasts, early adopters, CS/ML people who want to try new things. For a user who sees a language as a means to an end and who needs very robust code that can be shared code with others, established languages seem a safer choice. 


## Haskell

For many functional programming loyal fans, Haskell symbolizes purity and freedom. Its function is rich and powerful, but these functions are required to pay a certain price. It is impossible for you to master this language, because Haskell will force you to learn the full contents of functional programming. Think about the "Star Trek" Spock, the words he had said above is very representative, the perfect combination of logic and truth. The firmness of the purity of his character, which he has loved for generations. When Scala, Erlang, and Clojure also allow you to use a small number of command programming concepts, Haskell did not leave any room for maneuver. When you use Haskell to do I/O operations or state (state accumulate), you will encounter the challenge of the pure function language.


Haskell is a language from the beginning of the pure functional programming based on the idea of building, it combines some of the best functional language thinking, and focus on supporting inert processing.
Like Scala, Haskell is a strongly typed static type language. Its type model is based on inference theory (inferred) and is recognized as one of the most efficient types of functional languages. You will find that this type of system supports polymorphic semantics and helps people make a very neat and clear design.

### Advantages: 
- A very powerful language (in terms of features and capabilities) It has functionally the power of C++ , Ada and awk i.e. goodnees of Object oriented programming, easy Algorithm desiging and pattern matching. 
- Developement time is less : If you intend to implement the same functionalities in C/C++. It would take much more time. 

### Disadvantages: 
- Performance overhead : This language is not suitable for making time critical applications. 
- Learning time is high: Haskell is not as easy to grasp as other languages such as C or Pascal. 
- Lack of widespread implementations : This means that there are less platforms where the code writtne on Haskell can be run. Morever, there is no commercial Haskell implementation available, which means lack of support in case any problem is there in the compiler.


# How do you think programming languages will evolve in the future? State your thought. (A4paper at least 2 pages)

Software is only getting more and more complex and in coming days we need languages that address this complexity issue directly. I don't think we will see the emergence of 1 language addressing all concerns of developing a complex system. 

But I am quite excited at the upcoming trends of polytypic multistage programming. You write programs as DSLs in a host language, which then gets compiled to the target language optimized for the solution. 

Here is a presentation on Scala DSLs with Lightweight Modular Staging that compiles to JavaScript (Nada Amin on Scala DSLs with Lightweight Modular Staging and Compiling to JavaScript).

Another example from Scala is a DSL for nested data parallelism, written in Scala, that uses lightweight modular staging to transform into programs suitable for running on GPUs (Alexander Slesarenko on Lig). 

The benefit of the above approach is that the user doesn't have to learn a number of languages - rather she programs in one language and the compilers do the rest. But this host language has to be powerful and expressive enough to cater to such requirements. Lots of research are going on in this direction with Scala as the host language and I am quite hopeful of the outcome.

Another trend that also points to a good future is the use of more and more strongly typed languages that also has theorem proving capabilities built in. Languages like Agda, CoQ, Isabelle, Idris fall in this category. They not only help in modeling your system, but also helps you prove the correctness of the module to a large extent. Using such a language you eliminate a huge class of errors that creep in today's programming languages.

In the future:

-	Software will continue to get more complex and the cost of code maintenance will continue to be higher than cost of creating code
-	As the value in software increases, the need to create better software faster would increase
-	Server-side software would likely run on thousands of instances - distributed computing
-	Client-side software would likely run on millions of devices - deployment, updation, versioning issues
-	We will continue to get more, slower cores which would likely be under utilized because of Ahmdahl's law
-	Memory sizes will continue increasing (though latency will not improve much)

So one would like languages which provide:

-	easy readability
-	safety - inferred static typing and explicit dynamic typing - let the compiler help out as much as possible but bypass type-checking at compile time if the programmer says so
-	automatic memory management
-	more declarative than imperative - over-specifying leads to more bugs and less flexibility for the compiler / runtime to do things on behalf of the programmer.
    -	higher order functions
    -	list comprehensions
    -	pattern matching
-	a small, simple, powerful core - Lisp languages set the bar here
-	objects and classes for organizing concepts
-	first class support for fine-grained concurrency and distributed computing - Actor / CSP models
-	fast compilation
-	REPL support
-	deployment units with rich metadata which can be queried at runtime

The good news is that in the last 8-10 years many mainstream languages have started providing some of these facilities and many new languages have come up based on these ideas.
