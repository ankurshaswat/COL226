lookup([{X,T}|_],variable(X),T).
lookup([{Y,T2}|L2],variable(X),T):-lookup(L2,variable(X),T).


%union(L1,L2,Res).
union([],L2,L2).
union(L1,[],L1).
union([{X,T}|L1],L2,L3):-lookup(L2,variable(X),T2),union(L1,L2,L3).
union([{X,T}|L1],L2,[{X,T}|L3]):- ( \+ lookup(L2,variable(X),T2)),union(L1,L2,L3).

typeElaborates(Gamma,simpleDef(X,E),[{X,T}]):-hastype(Gamma,E,T).
typeElaborates(Gamma,sequentialDef(D1,D2),Uni):-typeElaborates(Gamma,D1,Gamma1),typeElaborates(Gamma1,D2,Gamma2),union(Gamma1,Gamma2,Uni).
typeElaborates(Gamma,parallelDef(D1,D2),Uni):-typeElaborates(Gamma,D1,Gamma1),typeElaborates(Gamma,D2,Gamma2),union(Gamma1,Gamma2,Uni).
typeElaborates(Gamma,localDef(D1,D2),Gamma2):-typeElaborates(Gamma,D1,Gamma1),union(Gamma,Gamma1,Uni),typeElaborates(Uni,D2,Gamma2).

hastype(Gamma,const(X),intT).
hastype(Gamma,true,boolT).
hastype(Gamma,false,boolT).

hastype(Gamma,plus(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,minus(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,multiply(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).

hastype(Gamma,divide(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,exponent(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,modulus(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,absolute(E),intT):-hastype(Gamma,E,intT).

hastype(Gamma,andd(E1,E2),boolT):-hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).
hastype(Gamma,or(E1,E2),boolT):-hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).
hastype(Gamma,not(E),boolT):-hastype(Gamma,E,boolT).
hastype(Gamma,implies(E1,E2),boolT):-hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).

hastype(Gamma,greater(E1,E2),boolT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,greaterEqual(E1,E2),boolT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,less(E1,E2),boolT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,lessEqual(E1,E2),boolT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,equal(E1,E2),boolT):-hastype(Gamma,E1,T),hastype(Gamma,E2,T).


hastype(Gamma,if_then_else(E0,E1,E2),T):-hastype(Gamma,E0,boolT),hastype(Gamma,E1,T),hastype(Gamma,E2,T).

hastype(Gamma,let_in(D,E),T):-typeElaborates(Gamma,D,Gamma2),union(Gamma,Gamma2,Uni),hastype(Uni,E,T).

hastype(Gamma,call(E1,E2),T2):-hastype(Gamma,E1,arrowT(T1,T2)),hastype(Gamma,E2,T1).
hastype(Gamma,func(variable(X),E1),arrowT(T1,T2)):-hastype([{X,T1}|Gamma],E1,T2).

hastype(Gamma,tuple([E1,E2]),cartesianT([T1,T2])):-hastype(Gamma,E1,T1),hastype(Gamma,E2,T2).
hastype(Gamma,tuple([E1|Erest]),cartesianT([T1|Trest])):-hastype(Gamma,E1,T1),hastype(Gamma,tuple(Erest),cartesianT(Trest)).

hastype(Gamma,projection(const(N),tuple([E1|Erest])),T):-N=:=0,hastype(Gamma,E1,T).
hastype(Gamma,projection(const(N),tuple([E1|Erest])),T):-N>0,hastype(Gamma,projection(const(N-1),tuple(Erest)),T).

hastype(Gamma,variable(X),T):-lookup(Gamma,variable(X),T).
