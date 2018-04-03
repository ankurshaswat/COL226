
type exp = Const of int
         | Var of string
         | Function of string * exp
         | Call of exp * exp;;

type closure = Clos of env * exp
and env = Func of (string * closure) list;;

let rec lookup (env,xx) = match (env,xx) with
  | (Func((x,closs)::xs),y) -> if(x=y) then closs else lookup(Func(xs),y);;

let rec evalKrivine state closureStack = match (state,closureStack) with
  | (Clos(env,))
  | (Clos(env,Var(x)),s) -> evalKrivine (lookup(env,x)) (s)
  | (Clos(Func(ll),Function(x,e1)),c::s) -> evalKrivine (Clos(Func((x,c)::ll),e1)) (s)
  | (Clos(env,Call(e1,e2)),s) -> evalKrivine (Clos(env,e1)) (Clos(env,e2)::s)
;;
