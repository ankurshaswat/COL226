open List;;
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

type closure = Clos1 of env * exp
and env = Tab of (string * closure) list;;

type closureOpcode = CLOS of env * exp
                   | CONST of int
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
            | Tuple1 of answer list
            | VClosure of ((string * answer) list) * string  *  answer;;

let rec lookup (env,xx) = match (env,xx) with
  | (Tab((x,Clos1(aa,bb))::xs),y) -> if(x=xx) then Clos1(aa,bb) else lookup(Tab(xs),y)
  | (Tab ([]),_) -> raise Error
;;


let rec formClosureList env lis = match (env,lis) with
  | (_,[]) -> []
  | (env,x::xs) -> (CLOS(env,x))::(formClosureList env xs);;

let rec getRest num lis = match (num,lis) with
  | (0,lis) -> []
  | (num,CONST(n)::xs) -> getRest (num-1) xs
  | (num,F::xs) -> getRest (num-1) xs
  | (num,T::xs) -> getRest (num-1) xs
  | (num,x::xs) -> [x]
  | (n, []) -> raise Error
;;

let rec cutOutList num lis = match (num,lis) with
  | (0,lis) -> []
  | (num,CONST(n)::xs) -> (cutOutList (num-1) xs) @ [Const(n)]
  | (num,T::xs) -> (cutOutList (num-1) xs) @ [T]
  | (num,F::xs) -> (cutOutList (num-1) xs) @ [F]
  | (_,_) -> raise Error
;;

let rec remove n lis = match (n,lis) with
  | (0,lis) -> lis
  | (n,x::xs) -> remove (n-1) xs;;


let rec convertList lis = match lis with
  | [] -> []
  | Tuple(l)::xs -> Tuple1(convertList l)::(convertList xs)
  | Const(n)::xs -> Const1(n)::(convertList xs)
  | T::xs -> T::(convertList xs)
  | F::xs -> F::(convertList xs)
;;

let rec evalKrivine state closureStack = match (state,closureStack) with
  (* ADD *)
  | (Clos1(env,Const(n)),(CLOS(env2,e)::ADD::s)) -> evalKrivine (Clos1(env2,e)) (CONST(n)::ADD::s)
  | (Clos1(env,Const(n1)),CONST(n2)::ADD::s) -> evalKrivine (Clos1(env,Const(n1+n2))) (s)
  | (Clos1(env,Plus(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (CLOS(env,e2)::ADD::s)

  (* MULTIPLICATION *)
  | (Clos1(env,Const(n1)),CONST(n2)::MULTIPLICATION::s) -> evalKrivine (Clos1(env,Const(n1*n2))) (s)
  | (Clos1(env,Const(n)),(CLOS(env2,e)::MULTIPLICATION::s)) -> evalKrivine (Clos1(env2,e)) (CONST(n)::MULTIPLICATION::s)
  | (Clos1(env,Multiplication(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (CLOS(env,e2)::MULTIPLICATION::s)

  (* GREATERTHAN *)
  | (Clos1(env,Const(b)),CONST(a)::GREATERTHAN::s) -> if (a>b) then evalKrivine (Clos1(env,T)) (s) else evalKrivine (Clos1(env,F)) (s)
  | (Clos1(env,Const(n)),(CLOS(env2,e)::GREATERTHAN::s)) -> evalKrivine (Clos1(env2,e)) (CONST(n)::GREATERTHAN::s)
  | (Clos1(env,GreaterThan(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (CLOS(env,e2)::GREATERTHAN::s)

  (* EQUAL *)
  | (Clos1(env,Const(b)),CONST(a)::EQUAL::s) -> if (a=b) then evalKrivine (Clos1(env,T)) (s) else evalKrivine (Clos1(env,F)) (s)
  | (Clos1(env,Const(n)),(CLOS(env2,e)::EQUAL::s)) -> evalKrivine (Clos1(env2,e)) (CONST(n)::EQUAL::s)
  | (Clos1(env,Equal(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (CLOS(env,e2)::EQUAL::s)

  (* TUPLE *)
  | (Clos1(env,Tuple(x::xs)),s) -> evalKrivine (Clos1(env,x)) (TUPLE((List.length xs)+1)::(formClosureList env xs)@s)
  | (Clos1(env,Const(n)),TUPLE(num)::s) -> (match (getRest num s) with
      (* [] ->  evalKrivine (Clos1(env,Tuple(cutOutList num s))) (remove num s) *)
        [] ->  Tuple1(convertList(cutOutList num s))
      | [CLOS(envv,expp)] -> evalKrivine (Clos1(envv,expp)) (TUPLE(num)::CONST(n)::s)
      | _ -> raise Error )

  (* IF THEN ELSE *)
  | (Clos1(env,IfThenElse(e1,e2,e3)),s) -> evalKrivine (Clos1(env,e1)) (CLOS(env,e2)::CLOS(env,e3)::ITE::s)
  | (Clos1(env,T),CLOS(env2,e2)::CLOS(env3,e3)::ITE::s) -> evalKrivine (Clos1(env2,e2)) s
  | (Clos1(env,F),CLOS(env2,e2)::CLOS(env3,e3)::ITE::s) -> evalKrivine (Clos1(env3,e3)) s

  | (Clos1(env,Var(x)),s) -> evalKrivine (lookup(env,x)) (s)
  | (Clos1(Tab(ll),Function(x,e1)),CLOS(envv,expp)::s) -> evalKrivine (Clos1(Tab((x,Clos1(envv,expp))::ll),e1)) (s)
  | (Clos1(env,Call(e1,e2)),s) -> evalKrivine (Clos1(env,e1)) (CLOS(env,e2)::s)

  | (Clos1(env,Const(n1)),s) -> Const1(n1)
  | (Clos1(env,T),s) -> T
  | (Clos1(env,F),s) -> F
  (* | (Clos1(env,Tuple(l)),s) -> Tuple1(convertList l) *)

  | (_,_) -> raise Error
;;
