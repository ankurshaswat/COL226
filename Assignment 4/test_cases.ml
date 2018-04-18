evalKrivine (Clos1(Tab(["z",Clos1(Tab([]),Const(3))]),Var("z"))) [];;

evalKrivine (Clos1(Tab([]),Plus(Plus(Const(2),Const(3)),Plus(Const(2),Const(3))))) [];;

evalKrivine (Clos1(Tab(["z",Clos1(Tab([]),Const(3))]),Plus(Const(2),Var("z")))) [];;

evalKrivine (Clos1(Tab([]),Call(Function("x",Plus(Var("x"),Const(1))),Const(2)))) [];;

evalKrivine (Clos1(Tab([]),Call(Function("x",Multiplication(Var("x"),Plus(Var("x"),Const(1)))),Const(2)))) [];;

evalKrivine (Clos1(Tab([]),Call(Function("x",Call(Function("d",Multiplication(Var("d"),Const(2))),Const(2))),Const(2)))) [];;

evalKrivine (Clos1(Tab([]),IfThenElse(GreaterThan(Const(8),Const(2)),Call(Function("x",Multiplication(Var("x"),Const(1))),Const(1)),Call(Function("x",Multiplication(Var("x"),Plus(Var("x"),Const(1)))),Const(2))))) [];;

evalKrivine (Clos1(Tab([]),IfThenElse(GreaterThan(Const(1),Const(2)),Call(Function("x",Multiplication(Var("x"),Const(1))),Const(1)),Call(Function("x",Multiplication(Var("x"),Plus(Var("x"),Const(1)))),Const(2))))) [];;

(* evalKrivine (Clos1(Tab([]),LetIn(Parallel[SimpleDef("a",Const(2))],Plus(Var("a"),Const(20))))) [];; *)

(* evalKrivine (Clos1(Tab([]),LetIn(Sequential[SimpleDef("a",Const(2))],Plus(Var("a"),Const(20))))) [];; *)

(* evalKrivine (Clos1(Tab([]),Projection(Tuple([Const(1),Const(2),Const(3)]),Const(2)))) [];; *)

(* evalKrivine (Clos1(Tab([]),Call(Function("x",LetIn(Parallel[SimpleDef("a",Const(2))],Plus(Var("a"),Var("x")))),Const(2)))) [];; *)

evalSECD [] (Table([])) (compile (Projection(Tuple([Const(12),Const(121),Const(33)]),Const(2)))) (Dump([]));;

evalSECD [] (Table([])) (compile (Letin(Parallel([SimpleDef("a",Const(1)),SimpleDef("b",Const(2)),SimpleDef("c",Const(3))]),Plus(Plus(Var("a"),Var("b")),Multiplication(Var("c"),Const(2)))),Multiplication(Const(2),Const(3)))) (Dump([]));;

evalSECD [] (Table([])) (compile (IfThenElse(GreaterThan(Const(4),Const(2)),Plus(Const(1),Const(3)),Plus(Const(1),Const(-4))))) (Dump([]));;

evalSECD [] (Table([])) (compile (Letin(Dec([Parallel([SimpleDef("f",F)]),Sequential([SimpleDef("a",Const(1)),SimpleDef("b",Const(2)),SimpleDef("c",Const(3))])]),Plus(Plus(Var("a"),Var("b")),Multiplication(Var("c"),Const(2)))),Multiplication(Const(2),Const(3))) (Dump([]));;
