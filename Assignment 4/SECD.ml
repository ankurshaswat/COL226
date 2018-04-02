exception Error;;

type exp = Const of int
         | Plus of exp * exp
         | Multiplication of exp * exp
         | GreaterThan of exp * exp
         | Equal of exp * exp
         | T
         | F
         | Tuple of exp list
         | IfThenElse of exp * exp * exp
         | Var of string
         | Function of string * exp
         | Call of exp * exp;;

let lookup (tab,x) = match (tab,x) with
  | (_,_) -> ([],Const(4)) ;;

let table_ex =[ "x",Const(5);"y",Const(6) ];;

type opcode = VAR of string
            | CONST of int
            | PLUS
            | MULTIPLICATION
            | GREATERTHAN
            | EQUAL
            | TT
            | FF
            | TUPLE of opcode list list
            | IF
            | COND of opcode list * (opcode list)
            | CLOSURE of string * opcode list
            | RET
            | APP;;

type answer = Const of int | T | F | Tuple of answer list | VClosure of ((string * answer) list) * string  *  opcode list;;

type table = Table of (string * answer) list;;

type closure = Closure of table * exp;;

type dump = Dump of (answer list * table * opcode list) list;;
let rec map f l = match l with
  | []-> []
  | x::xs -> (f x)::(map f xs);;

let rec invertMap data functionList = match functionList with
  | [] -> []
  | x::xs -> (x data)::(invertMap data xs);;

let rec compile e = match e with
  |Var(s)->[VAR(s)]
  |Const(n) -> [CONST(n)]
  |Plus (e1,e2)-> compile(e1)@compile(e2)@[PLUS]
  |Multiplication (e1,e2)->compile(e1)@compile(e2)@[MULTIPLICATION]
  |T -> [TT]
  |F -> [FF]
  |Equal(e1,e2) -> compile(e1)@compile(e2)@[EQUAL]
  |GreaterThan(e1,e2) -> compile(e1)@compile(e2)@[GREATERTHAN]
  |Tuple(l) -> [TUPLE(map compile l)]
  |IfThenElse(e0,e1,e2) -> [COND(compile e1,compile e2)]@compile(e2)@[IF]
  |Function (s,e) -> [CLOSURE(s,compile(e)@[RET])]
  | Call (e1,e2) -> compile(e1)@compile(e2)@[APP]
;;


let rec evalSECD stack environment opcodeL dump = match (stack,environment,opcodeL,dump) with
  | (s,e,CONST(n)::c,Dump(d)) -> evalSECD (Const(n)::s) e c (Dump(d))
  | (s1,Table(xx),VAR(s)::c,Dump(d)) -> evalSECD ((List.assoc s xx)::s1) (Table(xx)) c (Dump(d))
  | (Const(a)::Const(b)::s,e,PLUS::c,Dump(d)) -> evalSECD (Const(a+b)::s) e c (Dump(d))
  | (Const(a)::Const(b)::s,e,MULTIPLICATION::c,Dump(d)) -> evalSECD (Const(a*b)::s) e c (Dump(d))
  | (Const(a)::Const(b)::s,e,GREATERTHAN::c,Dump(d)) -> if (b>a) then evalSECD (T::s) e c (Dump(d)) else evalSECD (F::s) e c (Dump(d))
  | (Const(a)::Const(b)::s,e,EQUAL::c,Dump(d)) -> if (b=a) then evalSECD (T::s) e c (Dump(d)) else evalSECD (F::s) e c (Dump(d))
  | (s,e,TT::c,Dump(d)) -> evalSECD (T::s) e c (Dump(d))
  | (s,e,FF::c,Dump(d)) -> evalSECD (F::s) e c (Dump(d))
  | (s,e,TUPLE(l)::c,Dump(d)) -> evalSECD (Tuple(invertMap (Dump([])) (map (evalSECD [] e) l))::s) e c (Dump(d))
  | (T::tru::fals::s,e,IF::c,Dump(d)) ->  evalSECD (tru::s) e c (Dump(d))
  | (F::tru::fals::s,e,IF::c,Dump(d)) ->  evalSECD (fals::s) e c (Dump(d))
  | (s,e,COND(a,b)::c,Dump(d)) -> evalSECD ((evalSECD [] e a (Dump([])))::(evalSECD [] e b (Dump([])))::s) e c (Dump(d))
  | (s,Table(e),CLOSURE(x,opcodeL)::c,Dump(d)) -> evalSECD (VClosure(e,x,opcodeL)::s) (Table(e)) c (Dump(d))
  | (a::sPrime,Table(ePrime),RET::cPrime,Dump((s,Table(e),c)::d)) -> evalSECD (a::s) (Table(e)) c (Dump(d))
  | (a::VClosure(e,y,c)::s,Table(subs),APP::cPrime,Dump(d)) -> evalSECD [] (Table((y,a)::subs)) c (Dump((s,Table(subs),cPrime)::d))
  | _ -> raise Error
;;
