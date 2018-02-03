exception Error;;

type exp = Const of int
         |Abs of exp
         |Identifier of string
         |Addition of exp * exp
         |Subtraction of exp * exp
         |Multiplication of exp * exp
         |Division of exp * exp
         |Modulus of exp * exp
         |Exponentiation of exp * exp
         |Const1 of bool
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
         |Projection of int * exp list;;


type answer = Const of int | Const1 of bool | Tuple of answer list;;


let lookup s = 0;;

let rec pow (a,b) = match b with
    0 -> 1
  | 1 -> a
  | n -> a * pow (a,n-1);;

let rec map f l = match l with
  | []-> []
  | x::xs -> (f x)::(map f xs);;

let rec evalint e= match e with
  |Abs(e1) ->abs(evalint(e1))
  |Const n ->  n
  |Identifier s->lookup(s)
  |Addition (e1,e2)-> (evalint(e1)+evalint(e2))
  |Subtraction (e1,e2)-> (evalint(e1)-evalint(e2))
  |Multiplication (e1,e2)-> (evalint(e1)*evalint(e2))
  |Division (e1,e2)-> (evalint(e1)/evalint(e2))
  |Modulus (e1,e2)-> (evalint(e1) mod evalint(e2))
  |Exponentiation (e1,e2)-> (pow(evalint(e1),evalint(e2)))
  | _ -> raise Error ;;
(* ;; *)

let rec evalbool e= match e with
  |Not(e1) -> not(evalbool(e1))
  |Const1 b -> b
  |And(e1,e2) ->  evalbool(e1) && evalbool(e2)
  |Or(e1,e2) -> evalbool(e1) || evalbool(e2)
  |Implies(e1,e2) -> evalbool(Or(And(Const1(evalbool(e1)),Const1(evalbool(e2))),Not(Const1(evalbool(e1)))))
  |Equal(e1,e2) -> evalint(e1) = evalint(e2)
  |GreaterThan(e1,e2) -> evalint(e1) > evalint(e2)
  |LessThan(e1,e2) -> evalint(e1) < evalint(e2)
  |GreaterOrEqual(e1,e2) -> evalint(e1) >= evalint(e2)
  |LessOrEqual(e1,e2) -> evalint(e1) <= evalint(e2)
  | _ -> raise Error ;;
(* ;; *)

let rec eval e =match e with
  |Abs(e1) ->Const(evalint(Abs(e1)))
  |Const n -> Const n
  |Identifier s->Const(evalint(Identifier(s)))
  |Addition (e1,e2)->Const (evalint(Addition (e1,e2)))
  |Subtraction (e1,e2)->Const (evalint(Subtraction (e1,e2)))
  |Multiplication (e1,e2)->Const (evalint(Multiplication (e1,e2)))
  |Division (e1,e2)->Const (evalint(Division (e1,e2)))
  |Modulus (e1,e2)->Const (evalint(Modulus (e1,e2)))
  |Exponentiation (e1,e2)->Const (evalint(Exponentiation (e1,e2)))
  |Const1 b -> Const1 b
  |Not(e1) -> Const1(evalbool(Not(e1)))
  |And(e1,e2) -> Const1(evalbool(And(e1,e2)))
  |Or(e1,e2) -> Const1(evalbool(Or(e1,e2)))
  |Implies(e1,e2) -> Const1(evalbool(Implies(e1,e2)))
  |Equal(e1,e2) -> Const1(evalbool(Equal(e1,e2)))
  |GreaterThan(e1,e2) -> Const1(evalbool(GreaterThan(e1,e2)))
  |LessThan(e1,e2) -> Const1(evalbool(LessThan(e1,e2)))
  |GreaterOrEqual(e1,e2) -> Const1(evalbool(GreaterOrEqual(e1,e2)))
  |LessOrEqual(e1,e2) -> Const1(evalbool(LessOrEqual(e1,e2)))
  |Tuple(l) -> Tuple(map eval l)
  |Projection(0,x::xs) -> eval(x)
  |Projection(i,x::xs) -> eval(Projection(i-1,xs))
  |Projection(_,[])-> raise Error
;;
