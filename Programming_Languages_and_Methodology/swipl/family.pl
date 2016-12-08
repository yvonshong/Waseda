father(a,b).
father(c,d).
brother(a,c).
 
uncle(X,Y) :- brother(X,Z),father(Z,Y).