let f1 = Fact(Function("brother",[Function("ankit",[]);Function("ankit",[])]));;
let f1 = Fact(Function("brother",[Function("ankur",[]);Function("ankit",[])]));;
let f2 = Fact(Function("brother",[Function("ankur",[]);Function("swapnil",[])]));;

let f3 = Function("brother",[Function("ankur",[]);Function("swapnil",[])]);;

let prog = [f1;f2];;
unify [f3] prog;;

let f3 = Function("brother",[Function("ankur",[]);V(Var("X"))]);;
unify [f3] prog;;
