open List;;
exception Error;;

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

type closureOpcode = Clos of env * exp
                   | Const of int
                   | ADD
                   | MULTIPLICATION
                   | GREATERTHAN
                   | EQUAL
                   | T
                   | F
                   | TUPLE of int
                   | ITE ;;

type answer = Const1 of int
            | T
            | F
            | Tuple of answer list
            | VClosure of ((string * answer) list) * string  *  answer;;

let rec lookup (env,xx) = match (env,xx) with
  | (Func((x,Clos1(aa,bb))::xs),y) -> if(x=y) then Clos1(aa,bb) else lookup(Func(xs),y)
  | (Func ([]),_) -> raise Error
;;


let rec formClosureList env lis = match (env,lis) with
  | (_,[]) -> []
  | (env,x::xs) -> (Clos(env,x))::(formClosureList env xs);;

let rec getRest num lis = match (num,lis) with
  | (0,lis) -> []
  | (num,Const(n)::xs) -> getRest (num-1) xs
  | (num,F::xs) -> getRest (num-1) xs
  | (num,T::xs) -> getRest (num-1) xs
  | (num,x::xs) -> [x]
  | (n, []) -> raise Error
;;

let rec cutOutList num lis = match (num,lis) with
  | (0,lis) -> []
  | (num,Const(n)::xs) -> (cutOutList (num-1) xs) @ [Const1(n)]
  | (num,T::xs) -> (cutOutList (num-1) xs) @ [T]
  | (num,F::xs) -> (cutOutList (num-1) xs) @ [F]
  | (_,_) -> raise Error
;;

let rec evalKrivine state closureStack = match (state,closureStack) with
  (* ADD *)
  | (Clos1(env,Const(n)),(Clos(env2,e)::ADD::s)) -> evalKrivine (Clos1(env2,e)) (Const(n)::ADD::s)
  | (Clos1(env,Const(n1)),Const(n2)::ADD::s) -> Const1(n1+n2)
  | (Clos1(env,Add(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::ADD::s)

  (* MULTIPLICATION *)
  | (Clos1(env,Const(n1)),Const(n2)::MULTIPLICATION::s) -> Const1(n1*n2)
  | (Clos1(env,Const(n)),(Clos(env2,e)::MULTIPLICATION::s)) -> evalKrivine (Clos1(env2,e)) (Const(n)::MULTIPLICATION::s)
  | (Clos1(env,Multiplication(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::MULTIPLICATION::s)

  (* GREATERTHAN *)
  | (Clos1(env,Const(b)),Const(a)::GREATERTHAN::s) -> if (a>b) then T else F
  | (Clos1(env,Const(n)),(Clos(env2,e)::GREATERTHAN::s)) -> evalKrivine (Clos1(env2,e)) (Const(n)::GREATERTHAN::s)
  | (Clos1(env,GreaterThan(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::GREATERTHAN::s)

  (* EQUAL *)
  | (Clos1(env,Const(b)),Const(a)::EQUAL::s) -> if (a=b) then T else F
  | (Clos1(env,Const(n)),(Clos(env2,e)::EQUAL::s)) -> evalKrivine (Clos1(env2,e)) (Const(n)::EQUAL::s)
  | (Clos1(env,Equal(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::EQUAL::s)

  (* TUPLE *)
  | (Clos1(env,Tuple(x::xs)),s) -> evalKrivine (Clos1(env,x)) (TUPLE((List.length xs)+1)::(formClosureList env xs)@s)
  | (Clos1(env,Const(n)),TUPLE(num)::s) -> (match (getRest num s) with
        [] ->  Tuple(cutOutList num s)
      | [Clos(envv,expp)] -> evalKrivine (Clos1(envv,expp)) (TUPLE(num)::Clos(env,Const(n))::s)
      | _ -> raise Error )

  (* IF THEN ELSE *)
  | (Clos1(env,IfThenElse(e1,e2,e3)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::Clos(env,e3)::ITE::s)
  | (Clos1(env,T),Clos(env2,e2)::Clos(env3,e3)::ITE::s) -> evalKrivine (Clos1(env2,e2)) s
  | (Clos1(env,F),Clos(env2,e2)::Clos(env3,e3)::ITE::s) -> evalKrivine (Clos1(env3,e3)) s

  | (Clos1(env,Var(x)),s) -> evalKrivine (lookup(env,x)) (s)
  | (Clos1(Func(ll),Function(x,e1)),Clos(envv,expp)::s) -> evalKrivine (Clos1(Func((x,Clos1(envv,expp))::ll),e1)) (s)
  | (Clos1(env,Call(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (Clos(env,e2)::s)

  | (_,_) -> raise Error
;;
