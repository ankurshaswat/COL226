(* Function *)
let fact_edge_ab =
Fact(Function("edge", [Function("a", []); Function("b", [])]));;

let fact_edge_be =
Fact(Function("edge", [Function("b", []); Function("e", [])]));;

let fact_edge_ac =
Fact(Function("edge", [Function("a", []); Function("c", [])]));;

let fact_edge_ce =
Fact(Function("edge", [Function("c", []); Function("e", [])]));;

let fact_edge_ic =
Fact(Function("edge", [Function("i", []); Function("c", [])]));;

let fact_path_xx =
Fact(Function("path", [V(Var("X")); V(Var("X"))]));;

let rule_path_xy =
Rule(Function("path", [V(Var("X")); V(Var("Y"))]), [Function("edge", [V(Var("X")); V(Var("Y"))])]);;

let rule_path_xy' =
Rule(Function("path", [V(Var("X")); V(Var("Y"))]),
	[
		Function("edge", [V(Var("X")); V(Var("Z"))]);
		Function("path", [V(Var("Z")); V(Var("Y"))])
	]);;

let goal_aap =
Function("path", [Function("a", []); Function("a", [])]);;

let goal_ace =
Function("edge", [Function("a", []); Function("c", [])]);;

let goal_aie =
Function("edge", [Function("a", []); Function("i", [])]);;

let goal_Wcp =
Function("path", [V(Var("W")); Function("c", [])]);;

let goal_acp =
Function("path", [Function("a", []); Function("c", [])]);;

let goal_aWp =
Function("path", [Function("a", []); V(Var("W"))]);;

let goal_aWe =
Function("edge", [Function("a", []); V(Var("W"))]);;

let goal_aep =
Function("path", [Function("a", []); Function("e", [])]);;

let goal_aip =
Function("path", [Function("a", []); Function("i", [])]);;

let goal_Wip =
Function("path", [V(Var("W")); Function("i", [])]);;
let goal_W3p =
Function("path", [V(Var("W")); Function("e", [])]);;

let prog = [fact_edge_ab; fact_edge_be; fact_edge_ac; fact_edge_ce;
fact_edge_ic; fact_path_xx; rule_path_xy; rule_path_xy'];;


(* let prog2 = [fact_edge_ab; fact_edge_be; fact_path_xx; rule_path_xy; rule_path_xy'];; *)
(* let prog = [fact_edge_ab; fact_edge_be; fact_path_xx; rule_path_xy];; *)

(* let ss = mgu (Function("path", [V(Var("W")); Function("e", [])])) (Function("path", [V(Var("X")); V(Var("Y"))]));; *)
(* transformSingle 1 ss (Function("edge", [V(Var("X")); V(Var("Z"))]));; *)
(* let subAns=unify 0 [goal_W3p] prog2 prog2;; *)
let subAns=unify 0 [goal_aap] prog prog;;
(* let subAns=unify 0 [goal_W3p] prog prog;; *)
(* prolog [goal_W3p] subAns;; *)
