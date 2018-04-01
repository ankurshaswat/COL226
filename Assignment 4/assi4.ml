(* type var=Identifier of string;; *)

type exp = Const of int
         (* |Abs of exp
            |Addition of exp * exp
            |Subtraction of exp * exp
            |Multiplication of exp * exp
            |Division of exp * exp
            |Modulus of exp * exp
            |Exponentiation of exp * exp
            |T
            |F
            |Not of exp
            |And of exp * exp
            |Or of exp * exp
            |Implies of exp * exp
            |Equal of exp * exp
            |GreaterThan of exp * exp
            |LessThan of exp * exp
            |GreaterOrEqual of exp * exp
            |LessOrEqual of exp * exp
            |Tuple of exp list
            |Projection of int * exp list
            |IfThenElse exp * exp * exp *)
         | Var of string
         | Function of string * exp
         | Call of exp * exp;;

let lookup (tab,x) = match (tab,x) with
  | (_,_) -> ([],Const(4)) ;;


let table_ex =[ "x",Const(5);"y",Const(6) ];;

type opcode = VAR of string
            | CONST of int
            | CLOSURE of string * opcode list
            | RET
            | APP;;

type answer = Const of int | VClosure of ((string * answer) list) * string  *  opcode list;;

type table = Table of (string * answer) list;;

type closure = Closure of table * exp;;

type dump = Dump of (answer list * table * opcode list) list;;

let rec compile e = match e with
  |Var(s)->[VAR(s)]
  |Const(n) -> [CONST(n)]
  |Function (s,e) -> [CLOSURE(s,compile(e)@[RET])]
  | Call (e1,e2) -> compile(e1)@compile(e2)@[APP];;

let evalSECD stack environment opcodeL dump = match (stack,environment,opcodeL,dump) with
  | (s,e,CONST(n)::c,Dump(d)) -> (Const(n)::s,e,c,Dump(d))
  | (s,Table(e),CLOSURE(x,opcodeL)::c,Dump(d)) -> (VClosure(e,x,opcodeL)::s,Table(e),c,Dump(d))
  | (a::sPrime,Table(ePrime),RET::cPrime,Dump((s,Table(e),c)::d)) -> (a::s,Table(e),c,Dump(d))
  | (a::VClosure(e,y,c)::s,Table(subs),APP::cPrime,Dump(d)) -> ([],Table((y,a)::subs),c,Dump((s,Table(subs),cPrime)::d));;


let evalKrivine state closureStack = match (state,closureStack) with
  (* | (Closure(listClos,Function(x,e)),clos::s) -> (Closure(clos::listClos,e),s) *)
  (* | (Closure(listClos,Call(e1,e2)),s) -> (Closure(listClos,e1),Closure(listClos,e2)::s) *)
  (* | (Closure(listClos),Var(x)) ->  *)

(*
| (Closure(tab,Call(e1,e2)),s) -> (Closure(tab,e1),Closure(tab,e2)::s)
| (Closure(Table(tab),Function(x,e)),Closure(a,b)::s) -> (Closure(Table((x,VClosure(a,b))::tab),e),s)
| (Closure(tab,Var(x)),s) -> (lookup(tab,x),s)
;; *)

  | ((tab,Call(e1,e2)),s) -> ((tab,e1),(tab,e2)::s)
  | ((tab,Var(x)),s) -> (lookup(tab,x),s)
  | ((tab,Function(x,e)),cl::s) -> (((x,cl)::tab,e),s)
;;
