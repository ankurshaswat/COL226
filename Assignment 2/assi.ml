open List;;
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
         |Projection of int * exp list;;

type answer = Const of int | Const1 of bool | Tuple1 of answer list;;

let table =[ "x",Const(5);"y",Const1(true) ];;

let rho s = List.assoc s table;;

let rec pow (a,b) = match b with
    0 -> 1
  | 1 -> a
  | n -> a * pow (a,n-1);;

let rec map f l = match l with
  | []-> []
  | x::xs -> (f x)::(map f xs);;

let rec evalint lookup e= match e with
  |Abs(e1) ->abs(evalint lookup e1)
  |Const n ->  n
  |Identifier s-> (match lookup(s) with Const(n) -> n | _ -> raise Error)
  |Addition (e1,e2)-> ((evalint lookup e1)+(evalint lookup e2))
  |Subtraction (e1,e2)-> ((evalint lookup e1)-(evalint lookup e2))
  |Multiplication (e1,e2)-> ((evalint lookup e1)*(evalint lookup e2))
  |Division (e1,e2)-> ((evalint lookup e1)/(evalint lookup e2))
  |Modulus (e1,e2)-> ((evalint lookup e1) mod (evalint lookup e2))
  |Exponentiation (e1,e2)-> (pow((evalint lookup e1),(evalint lookup e2)))
  (* | _ -> raise Error *)
   ;;

let rec evalbool lookup e= match e with
  |Not(e1) -> not((evalbool lookup e1))
  |T -> true
  |F -> false
  |Identifier s-> (match lookup(s) with Const1(n) -> n | _ -> raise Error)
  |And(e1,e2) ->  (evalbool lookup e1) && (evalbool lookup e2)
  |Or(e1,e2) -> (evalbool lookup e1) || (evalbool lookup e2)
  |Implies(e1,e2) ->  (((evalbool lookup e1) && (evalbool lookup e2)) || not(evalbool lookup e1))
  |Equal(e1,e2) -> (evalint lookup e1) = (evalint lookup e2)
  |GreaterThan(e1,e2) -> (evalint lookup e1) > (evalint lookup e2)
  |LessThan(e1,e2) -> (evalint lookup e1) < (evalint lookup e2)
  |GreaterOrEqual(e1,e2) -> (evalint lookup e1) >= (evalint lookup e2)
  |LessOrEqual(e1,e2) -> (evalint lookup e1) <= (evalint lookup e2)
  (* | _ -> raise Error  *)
  ;;

let rec eval lookup e =match e with
  |Abs(e1) ->Const(evalint lookup (Abs(e1)))
  |Const n -> Const n
  |T -> Const1(evalbool lookup (T))
  |F -> Const1(evalbool lookup (F))
  |Identifier s->  lookup(s)
  |Addition (e1,e2)->Const (evalint lookup (Addition (e1,e2)))
  |Subtraction (e1,e2)->Const (evalint lookup (Subtraction (e1,e2)))
  |Multiplication (e1,e2)->Const (evalint lookup (Multiplication (e1,e2)))
  |Division (e1,e2)->Const (evalint lookup (Division (e1,e2)))
  |Modulus (e1,e2)->Const (evalint lookup (Modulus (e1,e2)))
  |Exponentiation (e1,e2)->Const (evalint lookup (Exponentiation (e1,e2)))
  |Not(e1) -> Const1(evalbool lookup (Not(e1)))
  |And(e1,e2) -> Const1(evalbool lookup (And(e1,e2)))
  |Or(e1,e2) -> Const1(evalbool lookup (Or(e1,e2)))
  |Implies(e1,e2) -> Const1(evalbool lookup (Implies(e1,e2)))
  |Equal(e1,e2) -> Const1(evalbool lookup (Equal(e1,e2)))
  |GreaterThan(e1,e2) -> Const1(evalbool lookup (GreaterThan(e1,e2)))
  |LessThan(e1,e2) -> Const1(evalbool lookup (LessThan(e1,e2)))
  |GreaterOrEqual(e1,e2) -> Const1(evalbool lookup (GreaterOrEqual(e1,e2)))
  |LessOrEqual(e1,e2) -> Const1(evalbool lookup (LessOrEqual(e1,e2)))
  |Tuple(l) -> Tuple1(map (eval lookup) l)
  |Projection(0,x::xs) -> eval lookup (x)
  |Projection(i,x::xs) -> eval lookup (Projection(i-1,xs))
  (* |Projection(_,_)-> raise Error *)
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
            |TT
            |FF
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
  |T -> [TT]
  |F -> [FF]
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
  |Projection(n,expl) -> [CONST(n)]@[TUPLE(map compile expl)]@[PROJECTION];;

let rec execute stack tab opcodeL = match (stack,tab,opcodeL) with
  |(Const(a)::s1,t,ABS::c) -> execute (Const(abs(a))::s1) t c
  |(s1,t,CONST(n)::c) -> execute (Const(n)::s1) t c
  |(s1,t,IDENTIFIER(s)::c) -> execute ((List.assoc s tab)::s1) t c
  |(Const(a)::Const(b)::s1,t,ADDITION::c) -> execute (Const(a+b)::s1) t c
  |(Const(a)::Const(b)::s1,t,SUBTRACTION::c) -> execute (Const(b-a)::s1) t c
  |(Const(a)::Const(b)::s1,t,MULTIPLICATION::c) -> execute (Const(a*b)::s1) t c
  |(Const(a)::Const(b)::s1,t,DIVISION::c) -> execute (Const(b/a)::s1) t c
  |(Const(a)::Const(b)::s1,t,MODULUS::c) -> execute (Const(b mod a)::s1) t c
  |(Const(a)::Const(b)::s1,t,EXPONENTIATION::c) -> execute (Const(pow(b,a))::s1) t c
  |(s1,t,TT::c) -> execute (Const1(true)::s1) t c
  |(s1,t,FF::c) -> execute (Const1(false)::s1) t c
  |(Const1(x)::s1,t,NOT::c) -> execute (Const1(not(x))::s1) t c
  |(Const1(y)::Const1(x)::s1,t,AND::c) -> execute (Const1(x&&y)::s1) t c
  |(Const1(y)::Const1(x)::s1,t,OR::c) -> execute (Const1(x||y)::s1) t c
  |(Const1(y)::Const1(x)::s1,t,IMPLIES::c) ->  execute (Const1(x && y || not(x))::s1) t c
  |(Const(y)::Const(x)::s1,t,EQUAL::c) -> execute (Const1(x=y)::s1) t c
  |(Const(y)::Const(x)::s1,t,GREATERTHAN::c) -> execute (Const1(x>y)::s1) t c
  |(Const(y)::Const(x)::s1,t,LESSTHAN::c) -> execute (Const1(x<y)::s1) t c
  |(Const(y)::Const(x)::s1,t,GREATEROREQUAL::c) -> execute (Const1(x>=y)::s1) t c
  |(Const(y)::Const(x)::s1,t,LESSOREQUAL::c) -> execute (Const1(x<=y)::s1) t c
  |(s1,t,TUPLE(l)::c) -> execute (Tuple1(map (execute [] t) l)::s1) t c
  |(Tuple1(x::xs)::Const(0)::s1,t,PROJECTION::c) -> execute (x::s1) t c
  |(Tuple1(x::xs)::Const(n)::s1,t,PROJECTION::c) -> execute (Tuple1(xs)::Const(n-1)::s1) t (PROJECTION::c)
  |(a::xs,t,[])-> a
  (* | (_,_,_) -> raise Error *)
  ;;

(* Examples *)

let z=Abs(Const(-3));;
eval rho (z);;
compile z;;
execute [] table (compile z);;
let z=Addition(Const(3),Const(4));;
eval rho (z);;
compile z;;
execute [] table (compile z);;
let z2=Subtraction(Const(3),Const(4));;
eval rho (z2);;
compile z2;;
execute [] table (compile z2);;
let z1=Multiplication(Const(3),Const(4));;
eval rho (z1);;
compile z1;;
execute [] table (compile z1);;
let z2=Division(Const(3),Const(4));;
eval rho (z2);;
compile z2;;
execute [] table (compile z2);;
let z2=Modulus(Const(3),Const(4));;
eval rho (z2);;
compile z2;;
execute [] table (compile z2);;
let z2=Exponentiation(Const(3),Const(4));;
eval rho (z2);;
compile z2;;
execute [] table (compile z2);;
let y=Subtraction(Const(10),Const(32));;
let x= Equal(y,z);;
eval rho (x);;
compile x;;
execute [] table (compile x);;
let x= GreaterOrEqual(y,z);;
eval rho (x);;
compile x;;
execute [] table (compile x);;
let x= GreaterThan(y,z);;
eval rho (x);;
compile x;;
execute [] table (compile x);;
let x= LessThan(y,z);;
eval rho (x);;
compile x;;
execute [] table (compile x);;
let x= LessOrEqual(y,z);;
eval rho (x);;
compile x;;
execute [] table (compile x);;
let a=Not(x);;
eval rho (a);;
compile a;;
execute [] table (compile a);;
let a3=Implies(T,T);;
eval rho (a3);;
compile a3;;
execute [] table (compile a3);;
let a3=And(T,F);;
eval rho (a3);;
compile a3;;
execute [] table (compile a3);;
let a3=Or(F,T);;
eval rho (a3);;
compile a3;;
execute [] table (compile a3);;
let a3=Implies(F,F);;
eval rho (a3);;
compile a3;;
execute [] table (compile a3);;
let ab=Tuple([a3;x;y;z;a]);;
eval rho (ab);;
compile ab;;
execute [] table (compile ab);;
let ac=Projection(2,[a3;x;y;z;a]);;
eval rho (ac);;
compile ac;;
execute [] table (compile ac);;
