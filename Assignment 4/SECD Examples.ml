let ex1 = Plus(Const(4),Const(5));;
let opco = compile ex1;;
evalSECD [] (Table([])) opco (Dump([]));;

let ex2 = Multiplication(Const(4),Const(5));;
let opco = compile ex2;;
evalSECD [] (Table([])) opco (Dump([]));;

let ex3 = Multiplication(ex1,ex2);;
let opco = compile ex3;;
evalSECD [] (Table([])) opco (Dump([]));;

let ex4 = GreaterThan(ex3,ex2);;
let opco = compile ex4;;
evalSECD [] (Table([])) opco (Dump([]));;

let notex4 = GreaterThan(ex2,ex3);;
let opco = compile ex4;;
evalSECD [] (Table([])) opco (Dump([]));;

let ex5 = Equal(ex3,ex2);;
let opco = compile ex5;;
evalSECD [] (Table([])) opco (Dump([]));;

let ex6 = Tuple([ex3;ex2]);;
let opco = compile ex6;;
evalSECD [] (Table([])) opco (Dump([]));;

let ex6 = IfThenElse(notex4,ex2,ex1);;
let opco = compile ex6;;
evalSECD [] (Table([])) opco (Dump([]));;

let ex7 = Var("x");;
let opco = compile ex7;;
evalSECD [] (Table["x",Const1(4)]) opco (Dump([]));;

let ex8 = Plus(Var("x"),Const(44));;
let ex9 = Function("x",ex8);;
let ex10 = Call (ex9,ex2);;
let opco = compile ex10;;
evalSECD [] (Table([])) opco (Dump([]));;
