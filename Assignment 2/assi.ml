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

(* type tuple = exp list;; *)


type answer = Const of int | Const1 of bool | Tuple2 of answer list;;


let lookup t s = 0;;

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
  |Projection(_,_)-> raise Error
;;

type opcode =CONST of int
            |ABS
            |IDENTIFIER of string
            |ADDITION
            |SUBTRACTION
            |MULTIPLICATION
            |DIVISION
            |MODULUS
            |EXPONENTIATION
            |CONST1 of bool
            |NOT
            |AND
            |OR
            |IMPLIES
            |EQUAL
            |GREATERTHAN
            |LESSTHAN
            |GREATEROREQUAL
            |LESSOREQUAL
            |TUPLE of opcode list list
            |PROJECTION;;

let rec compile e = match e with
  |Abs(e1) -> compile(e1)@[ABS]
  |Const(n) -> [CONST(n)]
  |Identifier s->[IDENTIFIER(s)]
  |Addition (e1,e2)-> compile(e1)@compile(e2)@[ADDITION]
  |Subtraction (e1,e2)->compile(e1)@compile(e2)@[SUBTRACTION]
  |Multiplication (e1,e2)->compile(e1)@compile(e2)@[MULTIPLICATION]
  |Division (e1,e2)->compile(e1)@compile(e2)@[DIVISION]
  |Modulus (e1,e2)->compile(e1)@compile(e2)@[MODULUS]
  |Exponentiation (e1,e2)->compile(e1)@compile(e2)@[EXPONENTIATION]
  |Const1 b -> [CONST1(b)]
  |Not(e1) -> compile(e1)@[NOT]
  |And(e1,e2) -> compile(e1)@compile(e2)@[AND]
  |Or(e1,e2) -> compile(e1)@compile(e2)@[OR]
  |Implies(e1,e2) ->compile(e1)@compile(e2)@[IMPLIES]
  |Equal(e1,e2) -> compile(e1)@compile(e2)@[EQUAL]
  |GreaterThan(e1,e2) -> compile(e1)@compile(e2)@[GREATERTHAN]
  |LessThan(e1,e2) -> compile(e1)@compile(e2)@[LESSTHAN]
  |GreaterOrEqual(e1,e2) -> compile(e1)@compile(e2)@[GREATEROREQUAL]
  |LessOrEqual(e1,e2) -> compile(e1)@compile(e2)@[LESSOREQUAL]
  |Tuple(l) -> [TUPLE(map compile l)]
  |Projection(n,expl) -> [CONST(n)]@[TUPLE(map compile expl)]@[PROJECTION]
;;

type stack = answer list;;

let rec execute stack table opcodeL = match (stack,table,opcodeL) with
  |(Const(a)::s1,t,ABS::c) -> execute (Const(abs(a))::s1) t c
  |(s1,t,CONST(n)::c) -> execute (Const(n)::s1) t c
  |(s1,t,IDENTIFIER(s)::c) -> execute (Const(lookup t s)::s1) t c
  |(Const(a)::Const(b)::s1,t,ADDITION::c) -> execute (Const(a+b)::s1) t c
  |(Const(a)::Const(b)::s1,t,SUBTRACTION::c) -> execute (Const(a-b)::s1) t c
  |(Const(a)::Const(b)::s1,t,MULTIPLICATION::c) -> execute (Const(a*b)::s1) t c
  |(Const(a)::Const(b)::s1,t,DIVISION::c) -> execute (Const(a/b)::s1) t c
  |(Const(a)::Const(b)::s1,t,MODULUS::c) -> execute (Const(a mod b)::s1) t c
  |(Const(a)::Const(b)::s1,t,EXPONENTIATION::c) -> execute (Const(pow(a,b))::s1) t c
  |(s1,t,CONST1(x)::c) -> execute (Const1(x)::s1) t c
  |(Const1(x)::s1,t,NOT::c) -> execute (Const1(not(x))::s1) t c
  |(Const1(x)::Const1(y)::s1,t,AND::c) -> execute (Const1(x&&y)::s1) t c
  |(Const1(x)::Const1(y)::s1,t,OR::c) -> execute (Const1(x||y)::s1) t c
  |(Const1(x)::Const1(y)::s1,t,IMPLIES::c) -> execute s1 t (CONST1(x)::CONST1(y)::AND::CONST1(x)::NOT::OR::c)
  |(Const1(x)::Const1(y)::s1,t,EQUAL::c) -> execute (Const1(x=y)::s1) t c
  |(Const1(x)::Const1(y)::s1,t,GREATERTHAN::c) -> execute (Const1(x>y)::s1) t c
  |(Const1(x)::Const1(y)::s1,t,LESSTHAN::c) -> execute (Const1(x<y)::s1) t c
  |(Const1(x)::Const1(y)::s1,t,GREATEROREQUAL::c) -> execute (Const1(x>=y)::s1) t c
  |(Const1(x)::Const1(y)::s1,t,LESSOREQUAL::c) -> execute (Const1(x<=y)::s1) t c


  |(a::xs,t,[])-> a
  (* |(Const1(x),t,[])-> Const1(x) *)
  (* | *)
;;
