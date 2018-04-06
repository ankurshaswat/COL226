let ex1 = Plus(Const(4),Const(5));;
evalKrivine (Clos1(Tab([]),ex1)) [];;

let ex2 = Multiplication(Const(4),Const(5));;
evalKrivine (Clos1(Tab([]),ex2)) [];;

let ex3 = Multiplication(ex1,ex2);;
evalKrivine (Clos1(Tab([]),ex3)) [];;

let ex4 = GreaterThan(ex3,ex2);;
evalKrivine (Clos1(Tab([]),ex4)) [];;

let notex4 = GreaterThan(ex2,ex3);;
evalKrivine (Clos1(Tab([]),notex4)) [];;

let ex5 = Equal(ex3,ex2);;
evalKrivine (Clos1(Tab([]),ex5)) [];;

let ex6 = Tuple([ex3;ex2]);;
evalKrivine (Clos1(Tab([]),ex6)) [];;

let ex6 = IfThenElse(notex4,ex2,ex1);;
evalKrivine (Clos1(Tab([]),ex6)) [];;

let ex7 = Var("x");;
evalKrivine (Clos1(Tab(["x",Clos1(Tab[],Const(4))]),ex7)) [];;

let ex8 = Plus(Var("x"),Const(44));;
let ex9 = Function("x",ex8);;
let ex10 = Call (ex9,ex2);;
evalKrivine (Clos1(Tab([]),ex10)) [];;
