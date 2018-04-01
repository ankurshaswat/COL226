
type exp = Const of int
         | Var of string
         | Function of string * exp
         | Call of exp * exp;;

(* let rec env x = (Const(3),env);; *)
type closure = Clos of env * exp
and env = Func of (string * closure) list;;

let rec lookup (env,xx) = match (env,xx) with
| (Func((x,closs)::xs),y) -> if(x=y) then closs else lookup(Func(xs),y);;


(* and  closure = Clos of exp * env ;; *)


(* type answer = Const of int | VClosure of ((string * answer) list) * string  *  opcode list;; *)

(* type table = Table of (string * answer) list;; *)

(* type closure = Closure of table * exp;; *)

(* type ;; *)
(* type var;; *)
(* type env;; *)
(* type state;; *)

let evalKrivine state closureStack = match (state,closureStack) with
  | (Clos(env,Var(x)),s) -> (lookup(env,x),s)
  | (Clos(Func(ll),Function(x,e1)),c::s) -> (Clos(Func((x,c)::ll),e1),s)
  | (Clos(env,Call(e1,e2)),s) -> (Clos(env,e1),Clos(env,e2)::s)
;;
