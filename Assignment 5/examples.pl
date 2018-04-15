anddhastype([],const(33),T).
anddhastype([],const(33),intT).
hastype([],true,T).
hastype([],true,boolT).

hastype([],plus(const(5),const(33)),T).
hastype([],plus(const(5),const(33)),intT).
hastype([],minus(const(5),const(33)),T).
hastype([],plus(const(5),true),T).
hastype([],plus(const(5),true),boolT).
hastype([],plus(const(5),true),intT).
hastype([],minus(const(5),true),T).
hastype([],absolute(const(3)),intT).
hastype([],absolute(const(3)),T).
hastype([],absolute(false),T).

hastype([],andd(true,const(4)),T).
hastype([],andd(true,const(4)),boolT).
hastype([],andd(true,false),boolT).
hastype([],andd(true,false),intT).
hastype([],andd(true,false),T).

hastype([{"x",intT},{"Y",boolT},{"L",intA},{"Q",intAA}],andd(true,variable("x")),T).
hastype([{"x",intT},{"Y",boolT},{"L",intA},{"Q",intAA}],andd(true,variable("x")),boolT).
hastype([{"x",intT},{"Y",boolT},{"L",intA},{"Q",intAA}],andd(true,variable("Y")),T).
hastype([{"x",intT},{"Y",boolT},{"L",intA},{"Q",intAA}],andd(true,variable("Y")),intT).
hastype([{"x",intT},{"Y",boolT},{"L",intA},{"Q",intAA}],andd(true,variable("Y")),boolT).

%lookup([{"x",intT},{"Y",intZ},{"L",intA},{"Q",intAA}],variable("x"),T).

%union([{"x",intT},{"Y",intZ}],[{"Y",intA},{"Q",intAA}],XX).

hastype([],or(true,false),boolT).
hastype([],or(true,false),T).
hastype([],not(true),boolT).
hastype([],not(true),T).
hastype([],not(true),intT).
hastype([],implies(true,false),boolT).
hastype([],implies(true,false),intT).
hastype([],implies(true,false),cartesianT).
hastype([],implies(true,false),T).

hastype([],greater(const(X),const(Y)),T).
hastype([],greater(const(X),const(Y)),intT).
hastype([],greater(const(X),const(Y)),boolT).
hastype([],greaterEqual(const(X),const(Y)),T).
hastype([],less(const(X),const(Y)),T).
hastype([],lessEqual(const(X),const(Y)),T).
hastype([],lessEqual(const(X),false),boolT).
hastype([],lessEqual(const(X),false),T).

hastype([],equal(plus(const(4),const(2)),multiply(const(33),const(22))),T).
hastype([],equal(plus(const(4),const(2)),multiply(const(33),const(22))),intT).
hastype([],equal(andd(true,false),not(true)),T).

hastype([],if_then_else(andd(true,const(4)),const(3),false),intT).
hastype([],if_then_else(andd(true,const(4)),const(3),false),T).
hastype([],if_then_else(andd(true,const(4)),const(3),const(33)),T).
hastype([],if_then_else(andd(true,const(4)),const(3),const(33)),intT).
hastype([],if_then_else(andd(true,true),const(3),const(33)),intT).

hastype([],let_in(simpleDef("x",true),variable("x")),T).
hastype([{"x",intT},{"Y",intZ},{"L",intA},{"Q",intAA}],let_in(simpleDef("x",true),variable("x")),T).
hastype([],let_in(parallelDef(simpleDef("x",true),simpleDef("y",const(4))),variable("y")),T).
hastype([],let_in(sequentialDef(simpleDef("x",true),simpleDef("y",const(4))),variable("x")),T).
hastype([],let_in(localDef(simpleDef("x",true),simpleDef("y",andd(variable("x"),true))),variable("y")),T).
hastype([],let_in(sequentialDef(simpleDef("x",true),simpleDef("x",const(4))),variable("x")),T).

hastype([],tuple([andd(true,false),const(3)]),cartesianT(XX)).
hastype([{"x",boolT}],tuple([andd(true,false),const(3),plus(const(4),const(33))]),cartesianT(XX)).
hastype([{"x",boolT}],tuple([andd(true,false),const(3),plus(const(4),const(33))]),T).
hastype([{"x",boolT}],tuple([andd(true,false),const(3),plus(const(4),const(33)),variable("x")]),cartesianT(XX)).

hastype([],projection(const(0),tuple([andd(true,false),const(3),plus(const(4),const(33))])),T).
hastype([],projection(const(2),tuple([andd(true,false),const(3),plus(const(4),const(33))])),T).
