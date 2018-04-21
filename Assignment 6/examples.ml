let f1 = Fact(Function("brother",[Function("ankit",[]);Function("ankit",[])]));;
let f2 = Fact(Function("brother",[Function("ankur",[]);Function("ankit",[])]));;
let f3 = Fact(Function("brother",[Function("ankur",[]);Function("swapnil",[])]));;

let prog = [f1;f2;f3];;

let g1 = Function("brother",[Function("ankur",[]);Function("swapnil",[])]);;
unify [g1] prog;;

let g2 = Function("brother",[Function("ankur",[]);V(Var("X"))]);;
unify [g1] prog;;

let g1 = Function("brother",[V(Var("Y"));V(Var("X"))]);;
let g2 = Function("brother",[V(Var("X"));V(Var("X"))]);;
unify [g1;g2] prog prog;;

let f1 = Fact(Function("brother",[Function("ankit",[]);Function("ankur",[])]));;
let f3 = Fact(Function("brother",[Function("ankur",[]);Function("swapnil",[])]));;
let r1 = Rule(Function("brother",[V(Var("X"));V(Var("Y"))]),[Function("brother",[V(Var("Y"));V(Var("X"))])]);;

let prog = [f1;f3;r1];;
(* let prog = [r1;f1;f3];; *)

let g1 = Function("brother",[V(Var("Y"));V(Var("X"))]);;
let g2 = Function("brother",[V(Var("X"));V(Var("Z"))]);;
unify [g1] prog prog;;

let g1 = Function("brother",[Function("ankur",[]);Function("ankit",[])]);;
unify [g1] prog prog;;
