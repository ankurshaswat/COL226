let f1 = Fact(Function("brother",[Function("ankit",[]);Function("ankit",[])]));;
let f2 = Fact(Function("brother",[Function("ankur",[]);Function("ankit",[])]));;
let f22 = Fact(Function("brother",[Function("ankur",[]);Function("swapnil",[])]));;

let prog = [f1;f2;f22];;

(* let f3 = Function("brother",[Function("ankur",[]);Function("swapnil",[])]);; *)
(* unify [f3] prog;; *)

(* unify [f3] prog;; *)
(* let f3 = Function("brother",[Function("ankur",[]);V(Var("X"))]);; *)

let f3 = Function("brother",[V(Var("Y"));V(Var("X"))]);;
unify [f3] prog;;
