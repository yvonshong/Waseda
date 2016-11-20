# Library Management System Design Report
Authored by: **SONG DAIWEI**  44161588-3
# Outline
I design this simple system using Java (maybe I will add Netbeans to implement GUI instead of Command Line). So I use a basic class named Account to be the father of member and libririan. Every book is a example of Book Class, and when member rewturn the book, he will pay for the bill.
## function point
- account management
    - libririan
    - member
        - rigister
        - login
- book
    - add/delete book
    - change detail of book
    - change status of book
    - borrowTime
- bill
    - calculate the bill
    - its owner

# Use Case Diagram
![userdiagram.jpg](userdiagram.jpg)

# Activity Diagram for Querying Book

![](act1diagram.jpg)

# Activity Diagram for Returning Book

![](act2diagram.jpg)

# State diagram for Book:

![](statediagram.jpg)

# Class Diagram for Library Management System
![](classdiagram.jpg)

# Deployment Diagram for Library management system
![](deploymentdiagram.jpg)
