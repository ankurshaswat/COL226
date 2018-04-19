%variables
hastype([{"X",intT},{variable("Y"),intT}],variable("X"),T).
hastype([{"X",boolT},{variable("Y"),intT}],variable("X"),T).

%Constants
hastype([],const(-652),T).
hastype([],true,T).

%arithmetic
hastype([],plus(minus(const(2),const(5)),divide(const(6),multiply(const(2),const(5)))),T).

%boolean
hastype([{"X",boolT}],andd(implies(or(variable("X"),false),true),implies(variable("X"),not(false))),T).

%comparison
hastype([{"X",boolT},{"Y",boolT}],or(andd(greater(const(-2),const(6)),less(const(3),const(100))),implies(equal(const(5),variable("Y")),variable("X"))),T).

%equality
hastype([],equal(tuple([tuple([const(1),const(3)]),true]),tup([const(1),const(3),true])),T).

%ifthenelse
hastype([{"X",boolT},{"Y",intT}],if_then_else(andd(variable("X"),greater(variable("Y"),const(0))),variable("Y"),variable("X")),T).

%letdine
hastype([{"Y",intT}],let_in(simpleDef("X",const(3)),plus(variable("Y"),variable("X"))),T).
hastype([{x,intT}],let_in(simpleDef(y,const(3)),multiply(variable(y),const(5))),T).

%abstraction
hastype([{x,boolT},{w,boolT}],func(variable(x),variable(w)),arrowT(boolT,boolT)).
hastype([{x,boolT},{w,boolT}],func(variable(x),variable(w)),arrowT(boolT,boolT)).

%application
hastype([{r,arrowT(boolT,boolT)},{s,boolT}],call(variable(r),variable(s)),boolT).
hastype([{r,arrowT(boolT,boolT)},{s,boolT},{s,boolT},{r,arrowT(boolT,boolT)}],call(variable(r),variable(s)),X).

%n-tuple
hastype([{x,boolT},{w,boolT}],tuple([variable(x),variable(w),andd(variable(x),variable(y))]),conjunction(boolT,boolT)).

%projection
hastype([{y,boolT},{z,boolT}],projection(const(1),tuple([variable(x),variable(w),andd(variable(x),variable(y))])),boolT).

%constructors
%hastype([(variable(r),typevar(boolT))],inl(variable(r)),disjunction(typevar(boolT),typevar(boolT))).
%hastype([(variable(r),typevar(boolT))],inl(variable(r)),X).
%hastype([(variable(r),typevar(boolT))],inr(variable(r)),disjunction(typevar(boolT),typevar(boolT))).

%caseanalysis
%hastype([(variable(t),typevar(boolT)),(variable(r),typevar(boolT))],case(inl(variable(t)),variable(r)),typevar(boolT)).
%hastype([(variable(t),typevar(boolT)),(variable(r),typevar(boolT))],case(inr(variable(t)),variable(r)),typevar(boolT)).


%typeelaborates

typeElaborates([],simpleDef("X",plus(const(3),const(4))),T).
typeElaborates([],simpleDef("Y",true),T).
typeElaborates([],parallelDef(simpleDef("X",const(3)),simpleDef("Y",true)),T).
typeElaborates([],parallelDef(simpleDef("X",const(3)),simpleDef("X",true)),T).
typeElaborates([],sequentialDef(simpleDef("X",multiply(const(31),const(20))),simpleDef("Y",true)),T).
typeElaborates([{"X",boolT},{"Y",intT}],localDef(simpleDef("X",const(31)),parallelDef(simpleDef("X",tuple([variable("Y"),const(3)])),simpleDef("Y",false))),T).
typeElaborates([{"X",boolT},{"Y",intT}],localDef(simpleDef("X",const(20)),parallelDef(simpleDef("X",const(3)),simpleDef("Y",false))),T).
typeElaborates([{x,intT}],simpleDef(y,const(9)),Gamma).

typeElaborates([{x,intT}],sequentialDef(simpleDef(z,true),simpleDef(y,false)),Gamma).

typeElaborates([{x,intT}],parallelDef(simpleDef(z,const(9)),simpleDef(y,const(0))),Gamma).

typeElaborates([{x,intT}],localDef(simpleDef(z,const(9)),simpleDef(y,const(4))),Gamma).

typeElaborates([{x,intT}],parallelDef(sequentialDef(simpleDef(z,const(8)),simpleDef(y,true)),simpleDef(y,false)),Gamma).

typeElaborates([{x,intT}],sequentialDef(parallelDef(simpleDef(z,const(45)),simpleDef(y,false)),simpleDef(y,const(8))),Gamma).
