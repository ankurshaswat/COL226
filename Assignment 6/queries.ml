
intermediate prog goal_ace;;
execute prog goal_ace;;

intermediate prog goal_aie;;
execute prog goal_aie;;

intermediate prog goal_aap;;
execute prog goal_aap;;

intermediate prog goal_Wcp;;
execute prog goal_Wcp;;

intermediate prog goal_acp;;
execute prog goal_acp;;

intermediate prog goal_aWe;;
execute prog goal_aWe;;

intermediate prog goal_aWp;;
execute prog goal_aWp;;

intermediate prog goal_aep;;
execute prog goal_aep;;

intermediate prog goal_aip;;
execute prog goal_aip;;




mgu (Function("const", [Var("N")])) (Function("const", [Const(3)]));;

let fact_const =
Fact(Function("hastype", [Var("Gamma"); Function("const",[Var("N")]); Function("intT", [])]));;

let rule_neg =
Rule(Function("hastype", [Var("Gamma"); Function("neg", [Var("E")]); Function("intT", [])]),
	[
		Function("hastype", [Var("Gamma"); Var("E"); Function("intT", [])])
	]);;

let rule_plus =
Rule(Function("hastype",
	[Var("Gamma"); Function("plus", [Var("E1"); Var("E2")]); Function("intT", [])]),
	[
		Function("hastype", [Var("Gamma"); Var("E1"); Function("intT", [])]);
		Function("hastype", [Var("Gamma"); Var("E2"); Function("intT", [])])
	]);;


let goal_const =
Function("hastype", [Var("Gamma"); Function("const", [Const(3)]); Var("T")]);;

let goal_const2 =
Function("hastype", [Var("Gamma"); Function("const", [Const(3)]); Function("intT", [])]);;

let goal_const_wrong =
Function("hastype", [Var("Gamma"); Function("const", [Const(3)]); Function("boolT", [])]);;

let goal_neg =
Function("hastype", [Var("Gamma"); Function("neg", [Function("const", [Const 3])]); Var("T")]);;

let goal_plus =
Function("hastype", [Var("Gamma"); Function("plus", [Function("const", [Const 2]); Function("const", [Const 3])]); Var("T")]);;


intermediate ([fact_const]) (goal_const);;
execute ([fact_const]) (goal_const);;
intermediate ([fact_const]) (goal_const2);;
execute ([fact_const]) (goal_const2);;
intermediate ([fact_const]) (goal_const_wrong);;
execute ([fact_const]) (goal_const_wrong);;


intermediate ([fact_const; rule_neg]) (goal_neg);;
execute ([fact_const; rule_neg]) (goal_neg);;
intermediate ([fact_const;rule_neg;rule_plus]) (goal_plus);;
execute ([fact_const;rule_neg;rule_plus]) (goal_plus);;

prolog ([fact_const;rule_neg;rule_plus]) ([goal_const;goal_neg;goal_plus; goal_const_wrong; goal_const2]);;
prolog_debug ([fact_const;rule_neg;rule_plus]) ([goal_const;goal_neg;goal_plus; goal_const_wrong; goal_const2]);;
