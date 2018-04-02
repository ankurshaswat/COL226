#use "submission.ml";;

(* Note:
 * Please make suitable changes to the shared test cases so that
 * the constructors match your signature definition.
 *)

(*--==Compile & Execute==--*)

let exp = Addition(Const(1),Const(2));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

let exp = Multiplication(Const(6),Const(6));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

let exp = Exponentiation(Const(2),Const(4));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

let exp = Division(Const(6),Const(3));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

let table =[ "iden1",Const(5);"iden2",Const1(true) ];;

let exp = Identifier("iden1");;
let opcodes = compile (exp);;
execute [] table opcodes;;

let exp = Identifier("iden2");;
let opcodes = compile (exp);;
execute [] table opcodes;;

let exp = Abs(Const(-1));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

let exp = Projection(2,([Const(12);Const(121);Const(33)]));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

let exp = Subtraction(Projection(2,([Const(2);Const(5);Const(8)])),Const(1));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

let exp = Modulus(Projection(2,([Const(2);Const(5);Const(8)])),Const(2));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

(* let exp = Or(
	Equal(Const(5),Const(5)),
	And(Equal(Subtraction(Const(2),Const(1)),Const(1)),
		Modulus(Projection(2,([Const(2);Const(5);Const(8)])),Const(2))
	)
);;
let opcodes = compile (exp);;
execute [] [] opcodes;; *)

let exp = And(T, F);;
let opcodes = compile (exp);;
execute [] [] opcodes;;

let exp = Implies(Not(Implies(Or(T, F), And(T, F))),Implies(And(T, F), Or(T, F)));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

let exp = GreaterOrEqual(Const(4),Const(2));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

let exp = LessOrEqual(Const(4),Const(2));;
let opcodes = compile (exp);;
execute [] [] opcodes;;
(*
let exp = Ifthenelse(Gtr(Const(4),Const(2)),Addition(Const(1),Const(3)),Sub(Const(1),Const(3)));;
let opcodes = compile (exp);;
execute [] [] opcodes;;

(* Lambda is a lambda function of type exp*exp and LetinEnd is a ternary operator of type exp*exp*exp *)
let exp = Apply(Lambda(Identifier("x"),LetinEnd(Para[Assgn(Identifier("a"),Const(2))],Addition(Identifier("a"),Identifier("x")))),Const(2));;
let opcodes = compile (exp);;
execute [] [] opcodes;; *)
