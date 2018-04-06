open List;;

type exp = Const of int
         | Add of exp * exp
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

type closure = Clos1 of env * exp
and env = Func of (string * closure) list;;

type closureOpcode = Clos of env * exp | Const of int | ADD  | MULTIPLICATION | GREATERTHAN | EQUAL | T | F | TUPLE of int | ITE ;;

type answer = Const of int | T | F | Tuple of answer list | VClosure of ((string * answer) list) * string  *  answer;;

let rec lookup (env,xx) = match (env,xx) with
  | (Func((x,Clos1(aa,bb))::xs),y) -> if(x=y) then Clos1(aa,bb) else lookup(Func(xs),y);;


let rec formClosureList env lis = match (env,lis) with
| (_,[]) -> []
| (env,x::xs) -> (Clos(env,x))::(formClosureList env xs);;

let rec evalKrivine state closureStack = match (state,closureStack) with
(* ADD *)
| (Clos1(env,Const(n)),(Clos(env2,e)::ADD::s)) -> evalKrivine (Clos1(env2,e)) (Const(n)::ADD::s)
  | (Clos1(env,Const(n)),Const(n2)::ADD::s) -> evalKrivine (Clos1(env,Const(n+n2))) s
  | (Clos1(env,Add(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::ADD::s)

(* MULTIPLICATION *)
| (Clos1(env,Const(n)),Const(n2)::MULTIPLICATION::s) -> evalKrivine (Clos1(env,Const(n*n2))) s
| (Clos1(env,Const(n)),(Clos(env2,e)::MULTIPLICATION::s)) -> evalKrivine (Clos1(env2,e)) (Const(n)::MULTIPLICATION::s)
| (Clos1(env,Multiplication(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::MULTIPLICATION::s)

(* GREATERTHAN *)
| (Clos1(env,Const(b)),Const(a)::GREATERTHAN::s) -> if (a>b) then evalKrivine (Clos1(env,T)) s else evalKrivine (Clos1(env,F)) s
| (Clos1(env,Const(n)),(Clos(env2,e)::GREATERTHAN::s)) -> evalKrivine (Clos1(env2,e)) (Const(n)::GREATERTHAN::s)
| (Clos1(env,GreaterThan(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::GREATERTHAN::s)

(* EQUAL *)
| (Clos1(env,Const(b)),Const(a)::EQUAL::s) -> if (a=b) then evalKrivine (Clos1(env,T)) s else evalKrivine (Clos1(env,F)) s
| (Clos1(env,Const(n)),(Clos(env2,e)::EQUAL::s)) -> evalKrivine (Clos1(env2,e)) (Const(n)::EQUAL::s)
| (Clos1(env,Equal(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::EQUAL::s)

(* TUPLE *)
| (Clos1(env,Tuple(x::xs)),s) -> evalKrivine (Clos1(env,x)) ((formClosureList env xs)@TUPLE((List.length xs)+1)::s)
|

(* IF THEN ELSE *)
| (Clos1(env,IfThenElse(e1,e2,e3)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::Clos(env,e3)::ITE::s)
| (Clos1(env,T),Clos(env2,e2)::Clos(env3,e3)::ITE::s) -> evalKrivine (Clos1(env2,e2)) s
| (Clos1(env,F),Clos(env2,e2)::Clos(env3,e3)::ITE::s) -> evalKrivine (Clos1(env3,e3)) s

  | (Clos1(env,Var(x)),s) -> evalKrivine (lookup(env,x)) (s)
  | (Clos1(Func(ll),Function(x,e1)),Clos(envv,expp)::s) -> evalKrivine (Clos1(Func((x,Clos1(envv,expp))::ll),e1)) (s)
  | (Clos1(env,Call(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::s)


  | (Clos1(env,Const(n)),s) -> Const(n)
  | (Clos1(env,T),s) -> T
  | (Clos1(env,F),s) -> F
;;
