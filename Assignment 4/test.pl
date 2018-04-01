likes(mary,food).
likes(mary,wine).
likes(john,wine).
likes(john,mary).

edge(a,b).
edge(b,d).
edge(a,c).
edge(b,e).
edge(c,e).
edge(e,g).
edge(c,f).
edge(f,h).
edge(i,c).

path(X,X).
path(X,Y):-edge(X,Z),path(Z,Y).
